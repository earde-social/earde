(* Community connections, slice 3 — the public "Connected communities" block.

   Three layers, each judged on its own: the read model that decides public
   visibility from current durable facts on both sides, the pure fragment that
   renders identity and nothing else, and the real community route that
   composes them — beside the authorized management surface, which keeps
   showing the accepted history the public block hides. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module R = Earde.Community_connected_communities_read_model

module P = Earde.Community_connected_communities_pages

module Store = Earde.Community_connections_store

module Cc = Earde.Community_connections

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let insert_community = Community_fixture.insert_community

let contains haystack needle = Html_assert.occurs haystack ~needle

let status_of = Http_fixture.status_of

let add_top_mod conn ~user ~community =
  exec conn "role fixture" Community_fixture.q_insert_moderator (user, community, "top_mod")

(* The three lifecycle drifts the sibling suites use, written straight to
   the durable columns. Each flips more than one column, because the
   communities table's own CHECKs require the combinations to stay coherent
   — what matters here is only that each leaves the community ineligible. *)
let make_private conn cid = exec conn "private" Community_fixture.q_make_private cid

let make_unlisted conn cid = exec conn "unlisted" Community_fixture.q_make_unlisted cid

let make_draft conn cid = exec conn "draft" Community_fixture.q_make_draft_state cid

(* communities_network_lifecycle_check allows a network community only two
   coherent shapes — draft+private+hidden, or published+public with
   indexable = discoverable — so a network community cannot be private
   while published. Turning it legacy first is how the visibility fact is
   exercised on its own. *)
let make_legacy conn cid = exec conn "legacy" Community_fixture.q_make_legacy cid

let make_legacy_private conn cid =
  let* () = make_legacy conn cid in
  make_private conn cid

(* One UPDATE, because the same CHECK refuses any intermediate state. *)
let q_restore_eligible =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE communities SET visibility = 'public', \
   onboarding_state = 'published', is_network_community = TRUE, \
   indexable = TRUE, discoverable = TRUE WHERE id = $1"

let restore conn cid = exec conn "restore eligibility" q_restore_eligible cid

(* ==================== the pure fragment ====================
   No database, no request, no session. *)

let c name slug = ({ name; slug } : P.connected_community)

let render communities = P.connected_communities_section ~communities

let empty_case =
  Alcotest.test_case "fragment: nothing publicly connected renders no card"
    `Quick (fun () ->
      Alcotest.(check string) "empty string, not an empty panel" ""
        (render []);
      (* And nothing that could read as a placeholder. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("absent: " ^ needle)
            false
            (contains (render []) needle))
        [ "Connected communities"; "ccc-section"; "None"; "No " ])

let identity_case =
  Alcotest.test_case "fragment: one heading, one name, one community link"
    `Quick (fun () ->
      let html = render [ c "Rust Users" "rust-users"; c "Ocaml" "ocaml" ] in
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("present: " ^ needle) true
            (contains html needle))
        [ ">Connected communities</h2>"
        ; "href='/c/rust-users'"; ">Rust Users</a>"
        ; "href='/c/ocaml'"; ">Ocaml</a>" ])

let escaping_case =
  Alcotest.test_case "fragment: names are escaped and an unaddressable slug \
                      renders no link" `Quick (fun () ->
      let html =
        render
          [ c "<script>alert(1)</script>" "safe-slug"
          ; c "Broken" "bad slug/with space" ]
      in
      Alcotest.(check bool) "no raw script tag" false
        (contains html "<script>alert(1)</script>");
      Alcotest.(check bool) "escaped instead" true
        (contains html "&lt;script&gt;");
      Alcotest.(check bool) "the addressable one links" true
        (contains html "href='/c/safe-slug'");
      Alcotest.(check bool) "the unaddressable one does not" false
        (contains html "bad slug/with space");
      Alcotest.(check bool) "but is still named" true
        (contains html ">Broken</p>"))

let vocabulary_case =
  Alcotest.test_case "fragment: no status, direction, note, actor, id, or \
                      management action can appear" `Quick (fun () ->
      let html = render [ c "Neighbour" "neighbour" ] in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("never rendered: " ^ needle)
            false (contains html needle))
        [ "pending"; "Pending"; "accepted"; "Accepted"; "rejected"
        ; "removed"; "requested"; "Requester"; "requester"; "reviewer"
        ; "Reviewed"; "note"; "Note"; "depends on"; "used by"
        ; "related ecosystem"; "<form"; "Remove"; "settings/connections" ])

let fragment_suite =
  [ empty_case; identity_case; escaping_case; vocabulary_case ]

(* ==================== the read model ==================== *)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM notifications \
       WHERE community_id IN \
         (SELECT id FROM communities WHERE slug LIKE 'cccc-%')"
    ; "DELETE FROM notifications \
       WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'cccc_%')"
    ; "DELETE FROM community_connection_audit_events \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'cccc-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'cccc-%')"
    ; "DELETE FROM community_connections \
       WHERE requester_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'cccc-%') \
          OR recipient_community_id IN \
               (SELECT id FROM communities WHERE slug LIKE 'cccc-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'cccc-%'"
    ; "DELETE FROM users WHERE username LIKE 'cccc_%'"
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

let error_str : R.error -> string = function
  | R.Invalid_community_slug -> "Invalid_community_slug"
  | R.Community_unavailable -> "Community_unavailable"
  | R.Inconsistent_data -> "Inconsistent_data"
  | R.Storage_error -> "Storage_error"

let load_ok label conn slug =
  let* r = R.load_for_community conn ~community_slug:slug in
  match r with
  | Ok communities ->
      Lwt.return (List.map R.community_slug communities)
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn slug =
  let* r = R.load_for_community conn ~community_slug:slug in
  match r with
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

(* Durable fixtures always go through the real store. *)
let connect conn ~actor ~requester ~recipient =
  match
    Cc.create_pending ~requester_community_id:requester
      ~recipient_community_id:recipient ~request_note:None
  with
  | Error _ -> Alcotest.fail "fixture: pending value refused"
  | Ok connection -> (
      let* r = Store.request conn ~actor_user_id:actor ~connection in
      match r with
      | Error _ -> Alcotest.fail "fixture: request refused"
      | Ok created -> Lwt.return (Store.created_connection_id created))

let accept conn ~actor ~id ~recipient =
  let* r =
    Store.review conn ~reviewer_user_id:actor ~connection_id:id
      ~recipient_community_id:recipient ~decision:Store.Accept
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.fail "fixture: accept refused"

let reject conn ~actor ~id ~recipient =
  let* r =
    Store.review conn ~reviewer_user_id:actor ~connection_id:id
      ~recipient_community_id:recipient ~decision:Store.Reject
  in
  match r with
  | Ok _ -> Lwt.return_unit
  | Error _ -> Alcotest.fail "fixture: reject refused"

let symmetric_case =
  db_case "read: one accepted connection is publicly visible from both \
           sides" (fun ~url:_ conn ->
      let* actor = insert_user conn "cccc_sym" in
      let* a = insert_community ~name:"Cccc Alpha" conn "cccc-sym-a" in
      let* b = insert_community ~name:"Cccc Beta" conn "cccc-sym-b" in
      let* id = connect conn ~actor ~requester:a ~recipient:b in
      let* () = accept conn ~actor ~id ~recipient:b in
      let* from_a = load_ok "from a" conn "cccc-sym-a" in
      Alcotest.(check (list string)) "a sees b" [ "cccc-sym-b" ] from_a;
      let* from_b = load_ok "from b" conn "cccc-sym-b" in
      Alcotest.(check (list string)) "b sees a" [ "cccc-sym-a" ] from_b;
      Lwt.return_unit)

let non_accepted_case =
  db_case "read: pending, rejected, and removed rows are never public"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "cccc_ne" in
      let* a = insert_community conn "cccc-ne-a" in
      let* b = insert_community conn "cccc-ne-b" in
      let* c = insert_community conn "cccc-ne-c" in
      let* d = insert_community conn "cccc-ne-d" in
      (* Pending. *)
      let* _ = connect conn ~actor ~requester:a ~recipient:b in
      (* Rejected. *)
      let* rid = connect conn ~actor ~requester:a ~recipient:c in
      let* () = reject conn ~actor ~id:rid ~recipient:c in
      (* Accepted then removed. *)
      let* did = connect conn ~actor ~requester:a ~recipient:d in
      let* () = accept conn ~actor ~id:did ~recipient:d in
      let* r =
        Store.remove conn ~actor_user_id:actor ~connection_id:did
          ~acting_community_id:a
      in
      (match r with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "fixture: removal refused");
      let* from_a = load_ok "from a" conn "cccc-ne-a" in
      Alcotest.(check (list string)) "nothing public" [] from_a;
      (* And from each counterpart's own side. *)
      let* () =
        Lwt_list.iter_s
          (fun slug ->
            let* rows = load_ok ("from " ^ slug) conn slug in
            Alcotest.(check (list string)) (slug ^ ": nothing public") []
              rows;
            Lwt.return_unit)
          [ "cccc-ne-b"; "cccc-ne-c"; "cccc-ne-d" ]
      in
      Lwt.return_unit)

let viewer_eligibility_case =
  db_case "read: an ineligible community publishes no connections at all"
    (fun ~url:_ conn ->
      let* actor = insert_user conn "cccc_ve" in
      let* a = insert_community conn "cccc-ve-a" in
      let* b = insert_community conn "cccc-ve-b" in
      let* id = connect conn ~actor ~requester:a ~recipient:b in
      let* () = accept conn ~actor ~id ~recipient:b in
      let* visible = load_ok "eligible" conn "cccc-ve-a" in
      Alcotest.(check (list string)) "visible while eligible"
        [ "cccc-ve-b" ] visible;
      (* Each of the three facts, one at a time; the counterpart never
         changes, so only the viewer's own state is being judged. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, drift) ->
            let* () = drift conn a in
            let* rows = load_ok label conn "cccc-ve-a" in
            Alcotest.(check (list string)) (label ^ ": hidden") [] rows;
            (* The counterpart still shows nothing either: the row is not
               mutated, but a's own ineligibility hides it there too. *)
            let* back = load_ok (label ^ " from b") conn "cccc-ve-b" in
            Alcotest.(check (list string))
              (label ^ ": counterpart excluded too") [] back;
            restore conn a)
          [ ("undiscoverable", make_unlisted)
          ; ("private", make_legacy_private)
          ; ("draft", make_draft) ]
      in
      (* Eligibility returning restores the very same accepted row. *)
      let* restored = load_ok "restored" conn "cccc-ve-a" in
      Alcotest.(check (list string)) "visible again" [ "cccc-ve-b" ]
        restored;
      Lwt.return_unit)

let counterpart_eligibility_case =
  db_case "read: an ineligible counterpart is excluded without touching the \
           connection" (fun ~url:_ conn ->
      let* actor = insert_user conn "cccc_ce" in
      let* a = insert_community conn "cccc-ce-a" in
      let* b = insert_community ~name:"Cccc Stays" conn "cccc-ce-b" in
      let* d = insert_community ~name:"Cccc Drifts" conn "cccc-ce-d" in
      let* idb = connect conn ~actor ~requester:a ~recipient:b in
      let* () = accept conn ~actor ~id:idb ~recipient:b in
      let* idd = connect conn ~actor ~requester:a ~recipient:d in
      let* () = accept conn ~actor ~id:idd ~recipient:d in
      let* both = load_ok "both" conn "cccc-ce-a" in
      (* lower(name) ASC: "Cccc Drifts" before "Cccc Stays". *)
      Alcotest.(check (list string)) "both counterparts"
        [ "cccc-ce-d"; "cccc-ce-b" ] both;
      let* () = make_legacy_private conn d in
      let* one = load_ok "one" conn "cccc-ce-a" in
      Alcotest.(check (list string)) "only the eligible counterpart"
        [ "cccc-ce-b" ] one;
      (* The durable row is untouched: the authorized management read still
         returns both accepted connections. *)
      let* accepted =
        Earde.Community_connections_read_model.list_accepted conn
          ~community_id:a
      in
      (match accepted with
      | Ok rows ->
          Alcotest.(check int) "both accepted rows survive" 2
            (List.length rows)
      | Error _ -> Alcotest.fail "management read failed");
      Lwt.return_unit)

let slug_case =
  db_case "read: a missing community and a non-segment slug are distinct \
           refusals" (fun ~url:_ conn ->
      let* () =
        Lwt_list.iter_s
          (fun slug ->
            load_expect ("invalid: " ^ slug) R.Invalid_community_slug conn
              slug)
          [ ""; "a/b"; "with space"; "tab\there" ]
      in
      load_expect "absent" R.Community_unavailable conn "cccc-nope")

let read_suite =
  [ symmetric_case; non_accepted_case; viewer_eligibility_case
  ; counterpart_eligibility_case; slug_case ]

(* ==================== the real community route ==================== *)

let gck_secret = "cccc-test-secret"

let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

let pipeline ?session_user_id ~url () =
  sql_pool url @@ Dream.set_secret gck_secret @@ Dream.memory_sessions
  @@ (fun handler request ->
       match session_user_id with
       | None -> handler request
       | Some uid ->
           let* () =
             Dream.set_session_field request "user_id" (string_of_int uid)
           in
           handler request)
  @@ Dream.router
       [ Dream.get "/c/:slug" Earde.Handlers.community_page_handler;
         Dream.get "/c/:slug/network"
           Earde.Handlers.community_network_handler;
         Dream.get "/c/:slug/settings/connections"
           Earde.Community_connections_handlers
           .make_connections_page_handler ]

let get ?session_user_id ~url target =
  let p = pipeline ?session_user_id ~url () in
  let* response = p (Dream.request ~method_:`GET ~target "") in
  let* body = Dream.body response in
  Lwt.return (status_of response, body)

let integration_case =
  db_case "route: the public block appears on both network pages, \
           disappears with eligibility, and never replaces management"
    (fun ~url conn ->
      let* top = insert_user conn "cccc_rt_top" in
      let* a = insert_community ~name:"Cccc Route Alpha" conn "cccc-rt-a" in
      let* b = insert_community ~name:"Cccc Route Beta" conn "cccc-rt-b" in
      let* () = add_top_mod conn ~user:top ~community:a in
      let* id = connect conn ~actor:top ~requester:a ~recipient:b in
      let* () = accept conn ~actor:top ~id ~recipient:b in
      (* Anonymously, from either side. *)
      let* status, body = get ~url "/c/cccc-rt-a/network" in
      Alcotest.(check int) "a: 200" 200 status;
      Alcotest.(check bool) "a: heading" true
        (contains body ">Connected communities</h2>");
      Alcotest.(check bool) "a: names b" true
        (contains body ">Cccc Route Beta</a>");
      Alcotest.(check bool) "a: links b" true
        (contains body "href='/c/cccc-rt-b'");
      let* _, body = get ~url "/c/cccc-rt-b/network" in
      Alcotest.(check bool) "b: names a" true
        (contains body ">Cccc Route Alpha</a>");
      (* Neither page states anything about the connection itself. *)
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            ("route never renders: " ^ needle)
            false (contains body needle))
        [ "cccc_rt_top"; "depends on"; "used by"; "related ecosystem" ];
      (* The connect shortcut follows the sidebar's existing top-mod-or-admin
         reading and nothing else — an ordinary visitor is never offered a
         way into the management flow. *)
      let* _, anon = get ~url "/c/cccc-rt-a/network" in
      Alcotest.(check bool) "anonymous: no connect shortcut" false
        (contains anon "settings/connections/new");
      let* _, tm = get ~session_user_id:top ~url "/c/cccc-rt-a/network" in
      Alcotest.(check bool) "top mod: connect shortcut" true
        (contains tm "href='/c/cccc-rt-a/settings/connections/new'");
      (* The community home carries the counted entry point instead: the
         same publicly visible set, one link away, with no counterpart
         named on it. *)
      let* status, home = get ~url "/c/cccc-rt-a" in
      Alcotest.(check int) "home: 200" 200 status;
      Alcotest.(check bool) "home: no list" false
        (contains home ">Connected communities</h2>");
      Alcotest.(check bool) "home: no counterpart named" false
        (contains home "Cccc Route Beta");
      Alcotest.(check bool) "home: counted row" true
        (contains home
           "<span class='launch-net-label'>Connected communities</span><span \
            class='launch-net-count'>1</span>");
      Alcotest.(check bool) "home: links the network page" true
        (contains home "href='/c/cccc-rt-a/network#communities'");
      (* The counterpart goes private: the public block disappears from the
         viewer's page and its count falls back to zero, while the durable
         row and its management view stay. *)
      let* () = make_unlisted conn b in
      let* _, body = get ~url "/c/cccc-rt-a/network" in
      Alcotest.(check bool) "a: block empty" true
        (contains body "No connected communities yet.");
      Alcotest.(check bool) "a: counterpart gone" false
        (contains body ">Cccc Route Beta</a>");
      let* _, home = get ~url "/c/cccc-rt-a" in
      Alcotest.(check bool) "home: row stays at zero" true
        (contains home
           "<span class='launch-net-label'>Connected communities</span><span \
            class='launch-net-count'>0</span>");
      let* status, body =
        get ~session_user_id:top ~url "/c/cccc-rt-a/settings/connections"
      in
      Alcotest.(check int) "management: 200" 200 status;
      Alcotest.(check bool) "management still lists the accepted history"
        true (contains body "href='/c/cccc-rt-b'");
      Lwt.return_unit)

let route_suite = [ integration_case ]

let suites =
  [ ("connected_communities_fragment", fragment_suite)
  ; ("connected_communities_reads", read_suite)
  ; ("connected_communities_route", route_suite)
  ]
