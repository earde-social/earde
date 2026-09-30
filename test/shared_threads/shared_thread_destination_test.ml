(* Shared threads as destination communities see them: feeds, sections,
   destination-context thread pages, comment participation and the
   moderation and privacy boundaries, over the real routed application. *)

let ( let* ) = Lwt.bind

let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let status_of = Http_fixture.status_of
let must = Html_assert.must
let must_not = Html_assert.must_not

(* --- destination community feeds: lifecycle, uniqueness, links --- *)

let feed_lifecycle_case =
  Shared_thread_http_fixture.db_case "an accepted placement appears exactly once in each destination \
           feed with destination links and provenance; every non-accepted \
           state is absent; disconnection and eligibility drift keep it; \
           origin privacy and removal end it; destination access still \
           gates" (fun ~url conn ->
      let* author, otop, dtop, o, d, post, connection = Shared_thread_http_fixture.fixture conn "df" in
      (* Second flat destination with its own reviewer. *)
      let* d2top = insert_user conn "sth_df_d2top" in
      let* d2 =
        Shared_thread_http_fixture.insert_community ~name:"Sth df Second" conn "sth-df-d2"
      in
      let* () = exec conn "flat d2" Shared_thread_fixture.q_set_sections (d2, false) in
      let* () = Shared_thread_http_fixture.add_top_mod conn ~user:d2top ~community:d2 in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      (* One post per non-accepted lifecycle state. *)
      let* p_pending =
        find conn "pending post" Shared_thread_http_fixture.q_insert_post ("Sth df pending", (o, author))
      in
      let* p_rejected =
        find conn "rejected post" Shared_thread_http_fixture.q_insert_post
          ("Sth df rejected", (o, author))
      in
      let* p_withdrawn =
        find conn "withdrawn post" Shared_thread_http_fixture.q_insert_post
          ("Sth df withdrawn", (o, author))
      in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      let* pl2 = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d2 () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:d2top ~placement:pl2 ~destination:d2 ()
      in
      let* _pending =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~note:"STH_NOTE_DF" ~post:p_pending
          ~destination:d ()
      in
      let* plr =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_rejected ~destination:d ()
      in
      let* () = Shared_thread_http_fixture.reject_seed conn ~reviewer:dtop ~placement:plr ~destination:d in
      let* plw =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_withdrawn ~destination:d ()
      in
      let* () = Shared_thread_http_fixture.withdraw_seed conn ~actor:author ~placement:plw ~origin:o in
      let dest_href =
        Earde.Components.canonical_thread_path "sth-df-d" post
          "Sth df thread"
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      Alcotest.(check int) "destination feed 200" 200 (status_of response);
      Shared_thread_http_fixture.count "shared row renders exactly once" body "Sth df thread" 1;
      Shared_thread_http_fixture.count "one provenance label" body "Shared from" 1;
      must body "Sth df Origin";
      must body dest_href;
      must_not body "Sth df pending";
      must_not body "Sth df rejected";
      must_not body "Sth df withdrawn";
      must_not body "STH_NOTE_DF";
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d2" anon in
      Shared_thread_http_fixture.count "once in the second destination" body "Sth df thread" 1;
      (* The origin feed is unchanged: the row, no provenance grammar. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-o" anon in
      must body "Sth df thread";
      must_not body "Shared from";
      must_not body "Shared with";
      (* Disconnection does not erase an accepted rendering. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:otop ~connection ~acting:o
      in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      must body "Sth df thread";
      (* Nor does losing discoverability/publication while still public. *)
      let* () = exec conn "unlist origin" Community_fixture.q_make_unlisted o in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      must body "Sth df thread";
      (* A private origin vanishes from EVERY destination immediately. *)
      let* () = exec conn "private origin" Community_fixture.q_make_private o in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      must_not body "Sth df thread";
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d2" anon in
      must_not body "Sth df thread";
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      must body "Sth df thread";
      (* Removal ends exactly its own destination; the sibling placement
         and the canonical origin surface survive. *)
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:pl ~acting:d in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d" anon in
      must_not body "Sth df thread";
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-d2" anon in
      must body "Sth df thread";
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-df-o" anon in
      must body "Sth df thread";
      (* The destination's own access rules still gate its feed. *)
      let* () = exec conn "private d2" Community_fixture.q_make_private d2 in
      let* response, _ = Shared_thread_http_fixture.get ~target:"/c/sth-df-d2" anon in
      Alcotest.(check int) "private destination is 404 to anon" 404
        (status_of response);
      let* dmem = insert_user conn "sth_df_dmem" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:dmem ~community:d2 in
      let* response, body =
        Shared_thread_http_fixture.get ~target:"/c/sth-df-d2" (Shared_thread_http_fixture.session ~url ~uid:dmem ())
      in
      Alcotest.(check int) "member still reads the private feed" 200
        (status_of response);
      must body "Sth df thread";
      Lwt.return_unit)

let feed_ordering_case =
  Shared_thread_http_fixture.db_case "all four sort modes and LIMIT/OFFSET order the COMBINED result \
           with canonical sort keys, no duplication across arms, and \
           explicit shared context on placement rows" (fun ~url conn ->
      ignore url;
      let* author, otop, dtop, o, d, post, _ = Shared_thread_http_fixture.fixture conn "dg" in
      let* own_a = find conn "own a" Shared_thread_http_fixture.q_insert_post ("Sth dg own a", (d, dtop)) in
      let* own_b = find conn "own b" Shared_thread_http_fixture.q_insert_post ("Sth dg own b", (d, dtop)) in
      let* shared2 =
        find conn "second shared" Shared_thread_http_fixture.q_insert_post ("Sth dg second", (o, author))
      in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      let* pl2 = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:shared2 ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl2 ~destination:d () in
      (* Distinct ages / activities / scores so every sort is decided. *)
      let* () = exec conn "age own_a" Shared_thread_http_fixture.q_age_post (own_a, 10) in
      let* () = exec conn "age own_b" Shared_thread_http_fixture.q_age_post (own_b, 1) in
      let* () = exec conn "age shared" Shared_thread_http_fixture.q_age_post (post, 5) in
      let* () = exec conn "age shared2" Shared_thread_http_fixture.q_age_post (shared2, 30) in
      let* () = exec conn "act shared" Shared_thread_http_fixture.q_set_activity (post, 1) in
      let* () = exec conn "act own_a" Shared_thread_http_fixture.q_set_activity (own_a, 2) in
      let* () = exec conn "act own_b" Shared_thread_http_fixture.q_set_activity (own_b, 3) in
      let* () = exec conn "act shared2" Shared_thread_http_fixture.q_set_activity (shared2, 4) in
      let* () = exec conn "v1" Shared_thread_http_fixture.q_vote (author, shared2, 1) in
      let* () = exec conn "v2" Shared_thread_http_fixture.q_vote (otop, shared2, 1) in
      let* () = exec conn "v3" Shared_thread_http_fixture.q_vote (dtop, shared2, 1) in
      let* () = exec conn "v4" Shared_thread_http_fixture.q_vote (author, own_a, 1) in
      let* () = exec conn "v5" Shared_thread_http_fixture.q_vote (otop, own_a, 1) in
      let* () = exec conn "v6" Shared_thread_http_fixture.q_vote (author, post, 1) in
      let feed sort limit offset =
        let* r = Earde.Db.get_posts_by_community conn d sort limit offset in
        Lwt.return (Shared_thread_http_fixture.ok "feed" r)
      in
      let* items = feed Earde.Db.Newest 20 0 in
      Alcotest.(check (list int)) "newest"
        [ own_b; post; own_a; shared2 ] (Shared_thread_http_fixture.feed_ids items);
      (* No duplication across the two arms. *)
      Alcotest.(check int) "four distinct rows" 4
        (List.length (List.sort_uniq compare (Shared_thread_http_fixture.feed_ids items)));
      (* Shared rows carry their context; own rows carry none. *)
      List.iter
        (fun (it : Earde.Db.feed_item) ->
          match it.Earde.Db.fi_shared with
          | Some ctx ->
              Alcotest.(check bool) "shared row is a placement row" true
                (List.mem it.Earde.Db.fi_post.id [ post; shared2 ]);
              Alcotest.(check string) "origin name travels"
                "Sth dg Origin" ctx.Earde.Db.fs_origin_name
          | None ->
              Alcotest.(check bool) "own row is the community's" true
                (List.mem it.Earde.Db.fi_post.id [ own_a; own_b ]))
        items;
      let* items = feed Earde.Db.Top 20 0 in
      Alcotest.(check (list int)) "top"
        [ shared2; own_a; post; own_b ] (Shared_thread_http_fixture.feed_ids items);
      let* items = feed Earde.Db.Hot 20 0 in
      Alcotest.(check (list int)) "hot"
        [ own_b; post; own_a; shared2 ] (Shared_thread_http_fixture.feed_ids items);
      let* items = feed Earde.Db.Active 20 0 in
      Alcotest.(check (list int)) "active"
        [ post; own_a; own_b; shared2 ] (Shared_thread_http_fixture.feed_ids items);
      (* Pagination applies to the combined result, never per arm. *)
      let* items = feed Earde.Db.Newest 2 0 in
      Alcotest.(check (list int)) "first page" [ own_b; post ]
        (Shared_thread_http_fixture.feed_ids items);
      let* items = feed Earde.Db.Newest 2 2 in
      Alcotest.(check (list int)) "second page" [ own_a; shared2 ]
        (Shared_thread_http_fixture.feed_ids items);
      Lwt.return_unit)

(* --- destination sections, uncategorized, statistics --- *)

let section_stats_case =
  Shared_thread_http_fixture.db_case "an accepted placement lives in its chosen destination section, \
           moves to Uncategorized when the section dies, counts in the \
           destination statistics, and never touches the origin section \
           or its statistics" (fun ~url conn ->
      let* author, _otop, dtop, o, d, _post, _ = Shared_thread_http_fixture.fixture conn "ds" in
      let* () = exec conn "origin sectioned" Shared_thread_fixture.q_set_sections (o, true) in
      let* osec = find conn "osec" Shared_thread_fixture.q_insert_section (o, "sth-ds-osec") in
      let* () = exec conn "dest sectioned" Shared_thread_fixture.q_set_sections (d, true) in
      let* sec1 = find conn "sec1" Shared_thread_fixture.q_insert_section (d, "sth-ds-sone") in
      let* sec2 = find conn "sec2" Shared_thread_fixture.q_insert_section (d, "sth-ds-stwo") in
      let* post2 =
        find conn "sectioned post" Shared_thread_http_fixture.q_insert_sectioned_post
          ("Sth ds sectioned", (o, author, osec))
      in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:post2 ~destination:d () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section:sec1 ~placement:pl
          ~destination:d ()
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      (* The chosen destination section carries it — exactly once, in
         destination context, with provenance. *)
      let* response, body = Shared_thread_http_fixture.get ~target:"/c/sth-ds-d/s/sth-ds-sone" anon in
      Alcotest.(check int) "section feed 200" 200 (status_of response);
      Shared_thread_http_fixture.count "once in the chosen section" body "Sth ds sectioned" 1;
      must body
        (Earde.Components.canonical_thread_path "sth-ds-d" post2
           "Sth ds sectioned");
      must body "Shared from";
      (* Not in a sibling section. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-ds-d/s/sth-ds-stwo" anon in
      must_not body "Sth ds sectioned";
      (* Destination statistics count it, with real activity. *)
      let* stats = Earde.Db.get_sections_with_stats conn d in
      let stats = Shared_thread_http_fixture.ok "dest stats" stats in
      let stat_of sid =
        match
          List.find_opt
            (fun ((s : Earde.Db.community_section), _, _) ->
              s.Earde.Db.section_id = sid)
            stats
        with
        | Some (_, n, act) -> (n, act)
        | None -> Alcotest.failf "missing section %d" sid
      in
      let n1, act1 = stat_of sec1 in
      Alcotest.(check int) "chosen section counts the placement" 1 n1;
      Alcotest.(check bool) "placement drives last activity" true
        (act1 <> None);
      let n2, _ = stat_of sec2 in
      Alcotest.(check int) "sibling section counts nothing" 0 n2;
      (* The overview renders the same count. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-ds-d" anon in
      must body "<span class='launch-sec-count'>1</span>";
      (* Origin section assignment and statistics are untouched. *)
      let* stored_section = find conn "origin section" Shared_thread_http_fixture.q_origin_section_of post2 in
      Alcotest.(check (option int)) "origin section assignment intact"
        (Some osec) stored_section;
      let* ostats = Earde.Db.get_sections_with_stats conn o in
      let ostats = Shared_thread_http_fixture.ok "origin stats" ostats in
      (match
         List.find_opt
           (fun ((s : Earde.Db.community_section), _, _) ->
             s.Earde.Db.section_id = osec)
           ostats
       with
       | Some (_, n, _) ->
           Alcotest.(check int) "origin section still counts one" 1 n
       | None -> Alcotest.fail "origin section vanished");
      (* Deleting the destination section releases the placement to
         Uncategorized (ON DELETE SET NULL) without ending it. *)
      let* () = exec conn "delete sec1" Shared_thread_fixture.q_delete_section sec1 in
      let* response, body = Shared_thread_http_fixture.get ~target:"/c/sth-ds-d/s/uncategorized" anon in
      Alcotest.(check int) "uncategorized 200" 200 (status_of response);
      Shared_thread_http_fixture.count "released into Uncategorized" body "Sth ds sectioned" 1;
      must body "Shared from";
      let* orphaned = Earde.Db.get_orphaned_count_and_activity conn d in
      let n, act = Shared_thread_http_fixture.ok "orphaned" orphaned in
      Alcotest.(check int) "orphaned count includes the placement" 1 n;
      Alcotest.(check bool) "orphaned activity is real" true (act <> None);
      (* Removal takes it out of the counts and the surface. *)
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:pl ~acting:d in
      let* orphaned = Earde.Db.get_orphaned_count_and_activity conn d in
      let n, _ = Shared_thread_http_fixture.ok "orphaned after removal" orphaned in
      Alcotest.(check int) "orphaned count falls back to zero" 0 n;
      let* response, _ = Shared_thread_http_fixture.get ~target:"/c/sth-ds-d/s/uncategorized" anon in
      Alcotest.(check int) "empty uncategorized is a 404 again" 404
        (status_of response);
      Lwt.return_unit)

let recent_knowledge_case =
  Shared_thread_http_fixture.db_case "Recent durable knowledge shows accepted placements through the \
           same combined community feed: destination link, provenance, \
           destination section, combined ordering, the five-item limit, \
           and origin privacy" (fun ~url conn ->
      let* author, _otop, dtop, o, d, post, _ = Shared_thread_http_fixture.fixture conn "dr" in
      let* () = exec conn "dest sectioned" Shared_thread_fixture.q_set_sections (d, true) in
      let* dsec = find conn "dsec" Shared_thread_fixture.q_insert_section (d, "sth-dr-sec") in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section:dsec ~placement:pl
          ~destination:d ()
      in
      let* () = exec conn "age shared" Shared_thread_http_fixture.q_age_post (post, 2) in
      let* local =
        find conn "local" Shared_thread_http_fixture.q_insert_post ("Sth dr local", (d, dtop))
      in
      let* () = exec conn "age local" Shared_thread_http_fixture.q_age_post (local, 1) in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response, body = Shared_thread_http_fixture.get ~target:"/c/sth-dr-d" anon in
      Alcotest.(check int) "overview 200" 200 (status_of response);
      (* The shared row: destination-context link, destination section,
         compact provenance; the local row keeps its byte-identical
         legacy link. *)
      must body
        (Earde.Components.canonical_thread_path "sth-dr-d" post
           "Sth dr thread");
      must body "Shared from";
      must body "sth-dr-sec";
      must body (Printf.sprintf "href='/p/%d'" local);
      (* Combined ordering: the newer local row leads. *)
      let pos needle =
        match Html_assert.index_of body needle with
        | Some i -> i
        | None -> Alcotest.failf "missing: %s" needle
      in
      Alcotest.(check bool) "newer local row leads the panel" true
        (pos "Sth dr local" < pos "Sth dr thread");
      (* Origin privacy removes it immediately. *)
      let* () = exec conn "private origin" Community_fixture.q_make_private o in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-dr-d" anon in
      must_not body "Sth dr thread";
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      (* The five-item limit applies AFTER combination. *)
      let* () =
        Lwt_list.iter_s
          (fun i ->
            let* _ =
              find conn "filler" Shared_thread_http_fixture.q_insert_post
                (Printf.sprintf "Sth dr filler %d" i, (d, dtop))
            in
            Lwt.return_unit)
          [ 1; 2; 3; 4; 5 ]
      in
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-dr-d" anon in
      Shared_thread_http_fixture.count "exactly five recent rows" body "launch-postrow" 5;
      must_not body "Sth dr thread";
      Lwt.return_unit)

(* --- destination-context thread pages --- *)

let context_url_case =
  Shared_thread_http_fixture.db_case "the destination thread URL renders the one canonical discussion \
           under the destination shell with provenance, an origin-pointing \
           canonical link, and noindex; every invalid context collapses \
           into the pre-existing redirect-or-404 behavior" (fun ~url conn ->
      let* author, _otop, dtop, o, d, post, _ = Shared_thread_http_fixture.fixture conn "dc" in
      let* () = exec conn "dest sectioned" Shared_thread_fixture.q_set_sections (d, true) in
      let* dsec = find conn "dsec" Shared_thread_fixture.q_insert_section (d, "sth-dc-sec") in
      let* _comment = find conn "comment" Shared_thread_fixture.q_insert_comment (post, author) in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section:dsec ~placement:pl
          ~destination:d ()
      in
      let canonical =
        Earde.Components.canonical_thread_path "sth-dc-o" post "Sth dc thread"
      in
      let dest_path =
        Earde.Components.canonical_thread_path "sth-dc-d" post "Sth dc thread"
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      (* The origin page behaves exactly as before. *)
      let* response, body = Shared_thread_http_fixture.get ~target:canonical anon in
      Alcotest.(check int) "origin page 200" 200 (status_of response);
      must body
        (Printf.sprintf "<link rel='canonical' href='%s'>" canonical);
      must body "kept";
      must_not body "Shared from";
      must_not body "content='noindex'";
      (* The destination context: same canonical post and comments, the
         destination shell and section, the provenance label, the
         origin-pointing canonical, noindex, and no Share entry point. *)
      let* response, body = Shared_thread_http_fixture.get ~target:dest_path anon in
      Alcotest.(check int) "destination page 200" 200 (status_of response);
      must body "Sth dc thread";
      must body "kept";
      must body "Shared from";
      must body "href='/c/sth-dc-o'";
      must body
        (Printf.sprintf "<link rel='canonical' href='%s'>" canonical);
      must body "content='noindex'";
      must body "Sth dc Dest";
      must body "/c/sth-dc-d/s/sth-dc-sec";
      must_not body "Share with a community";
      must_not body "sth_dc_dtop";
      (* Wrong descriptive slug 301s WITHIN the destination context. *)
      let* response, _ =
        Shared_thread_http_fixture.get ~target:(Printf.sprintf "/c/sth-dc-d/t/%d-junk" post) anon
      in
      Shared_thread_http_fixture.check_301 "destination slug correction" dest_path response;
      (* A logged-in non-member's join CTA targets the DESTINATION. *)
      let* stranger = insert_user conn "sth_dc_str" in
      let* _, body =
        Shared_thread_http_fixture.get ~target:dest_path
          (Shared_thread_http_fixture.session ~url ~uid:stranger ~username:"sth_dc_str" ())
      in
      must body "Join /c/sth-dc-d";
      must body (Printf.sprintf "name='community_id' value='%d'" d);
      (* A destination member's composer carries the closed context field. *)
      let* dmem = insert_user conn "sth_dc_dmem" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:dmem ~community:d in
      let* _, body = Shared_thread_http_fixture.get ~target:dest_path (Shared_thread_http_fixture.session ~url ~uid:dmem ()) in
      must body "name='context_community' value='sth-dc-d'";
      (* Every invalid context is the pre-existing behavior, with no
         observable difference between an unrelated community and every
         inactive placement state. *)
      let* _x = Shared_thread_http_fixture.insert_community ~name:"Sth dc X" conn "sth-dc-x" in
      let* response, _ =
        Shared_thread_http_fixture.get
          ~target:(Printf.sprintf "/c/sth-dc-x/t/%d-sth-dc-thread" post)
          anon
      in
      Shared_thread_http_fixture.check_301 "unrelated community" canonical response;
      let state_post label transition =
        let* p =
          find conn label Shared_thread_http_fixture.q_insert_post ("Sth dc " ^ label, (o, author))
        in
        let* placement = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p ~destination:d () in
        let* () = transition p placement in
        let* response, _ =
          Shared_thread_http_fixture.get
            ~target:
              (Earde.Components.canonical_thread_path "sth-dc-d" p
                 ("Sth dc " ^ label))
            anon
        in
        Shared_thread_http_fixture.check_301 (label ^ " context")
          (Earde.Components.canonical_thread_path "sth-dc-o" p
             ("Sth dc " ^ label))
          response;
        Lwt.return_unit
      in
      let* () = state_post "pend" (fun _ _ -> Lwt.return_unit) in
      let* () =
        state_post "rej" (fun _ placement ->
            Shared_thread_http_fixture.reject_seed conn ~reviewer:dtop ~placement ~destination:d)
      in
      let* () =
        state_post "wd" (fun _ placement ->
            Shared_thread_http_fixture.withdraw_seed conn ~actor:author ~placement ~origin:o)
      in
      let* () =
        state_post "rm" (fun _ placement ->
            let* () =
              Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section:dsec ~placement
                ~destination:d ()
            in
            Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement ~acting:d)
      in
      (* Origin privacy: the destination context stops rendering — a
         viewer who may read the origin gets the canonical redirect, one
         who may not gets the generic 404. *)
      let* () = exec conn "private origin" Community_fixture.q_make_private o in
      let* response, _ = Shared_thread_http_fixture.get ~target:dest_path anon in
      Alcotest.(check int) "private origin is 404 to anon" 404
        (status_of response);
      let* response, _ = Shared_thread_http_fixture.get ~target:dest_path (Shared_thread_http_fixture.session ~url ~uid:author ()) in
      Shared_thread_http_fixture.check_301 "origin member still converges on canonical" canonical
        response;
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      (* A private destination renders only for its authorized viewers;
         everyone else keeps the redirect. *)
      let* () = exec conn "private dest" Community_fixture.q_make_private d in
      let* response, _ = Shared_thread_http_fixture.get ~target:dest_path anon in
      Shared_thread_http_fixture.check_301 "anon and the private destination" canonical response;
      let* response, body = Shared_thread_http_fixture.get ~target:dest_path (Shared_thread_http_fixture.session ~url ~uid:dmem ()) in
      Alcotest.(check int) "private-destination member renders" 200
        (status_of response);
      must body "Shared from";
      let* () = exec conn "restore dest" Shared_thread_fixture.q_make_eligible d in
      let* () = exec conn "re-section dest" Shared_thread_fixture.q_set_sections (d, true) in
      (* Long names stay safe end to end. *)
      let long_title = "Sth dc " ^ String.make 80 'x' in
      let* p_long = find conn "long" Shared_thread_http_fixture.q_insert_post (long_title, (o, author)) in
      let* placement = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_long ~destination:d () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~section:dsec ~placement
          ~destination:d ()
      in
      let* response, body =
        Shared_thread_http_fixture.get
          ~target:
            (Earde.Components.canonical_thread_path "sth-dc-d" p_long
               long_title)
          anon
      in
      Alcotest.(check int) "long title renders" 200 (status_of response);
      must body "Shared from";
      Lwt.return_unit)

(* --- comment participation: the capability and the POST --- *)

let comment_participation_case =
  Shared_thread_http_fixture.db_case "one participation capability governs composer and POST: origin \
           membership or a currently readable accepted destination \
           membership, minus tombstone and every ban; the redirect keeps \
           the destination context only while it is still readable"
    (fun ~url conn ->
      let* author, _otop, dtop, o, d, post, connection = Shared_thread_http_fixture.fixture conn "dp" in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      (* A second accepted destination and a pending one. *)
      let* d3top = insert_user conn "sth_dp_d3top" in
      let* d3 = Shared_thread_http_fixture.insert_community ~name:"Sth dp Third" conn "sth-dp-d3" in
      let* () = exec conn "flat d3" Shared_thread_fixture.q_set_sections (d3, false) in
      let* () = Shared_thread_http_fixture.add_top_mod conn ~user:d3top ~community:d3 in
      let* _ = Shared_thread_fixture.connect conn ~actor:author o d3 in
      let* pl3 = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d3 () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:d3top ~placement:pl3 ~destination:d3 ()
      in
      let* d2 = Shared_thread_http_fixture.insert_community ~name:"Sth dp Second" conn "sth-dp-d2" in
      let* () = exec conn "flat d2" Shared_thread_fixture.q_set_sections (d2, false) in
      let* _ = Shared_thread_fixture.connect conn ~actor:author o d2 in
      let* _pending = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d2 () in
      (* Posts frozen in each non-participating state. *)
      let* p_r = find conn "p_r" Shared_thread_http_fixture.q_insert_post ("Sth dp rej", (o, author)) in
      let* plr = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_r ~destination:d () in
      let* () = Shared_thread_http_fixture.reject_seed conn ~reviewer:dtop ~placement:plr ~destination:d in
      let* p_w = find conn "p_w" Shared_thread_http_fixture.q_insert_post ("Sth dp wd", (o, author)) in
      let* plw = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_w ~destination:d () in
      let* () = Shared_thread_http_fixture.withdraw_seed conn ~actor:author ~placement:plw ~origin:o in
      let* p_m = find conn "p_m" Shared_thread_http_fixture.q_insert_post ("Sth dp rm", (o, author)) in
      let* plm = Shared_thread_http_fixture.seed_request conn ~actor:author ~post:p_m ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:plm ~destination:d () in
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:plm ~acting:d in
      (* The cast. *)
      let* dmem = insert_user conn "sth_dp_dmem" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:dmem ~community:d in
      let* pmem = insert_user conn "sth_dp_pmem" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:pmem ~community:d2 in
      let* dban = insert_user conn "sth_dp_dban" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:dban ~community:d in
      let* () = exec conn "ban dban in d" Shared_thread_http_fixture.q_ban_community (dban, d) in
      let* oban = insert_user conn "sth_dp_oban" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:oban ~community:d in
      let* () = exec conn "ban oban in o" Shared_thread_http_fixture.q_ban_community (oban, o) in
      let* gban = insert_user conn "sth_dp_gban" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:gban ~community:o in
      let* () = exec conn "gban" Shared_thread_http_fixture.q_ban_global gban in
      let* stranger = insert_user conn "sth_dp_str" in
      (* The capability matrix. *)
      let* () = Shared_thread_http_fixture.may_comment "origin member" conn ~user:author ~post true in
      let* () = Shared_thread_http_fixture.may_comment "accepted-destination member" conn ~user:dmem ~post true in
      let* () = Shared_thread_http_fixture.may_comment "pending grants nothing" conn ~user:pmem ~post false in
      let* () = Shared_thread_http_fixture.may_comment "rejected grants nothing" conn ~user:dmem ~post:p_r false in
      let* () = Shared_thread_http_fixture.may_comment "withdrawn grants nothing" conn ~user:dmem ~post:p_w false in
      let* () = Shared_thread_http_fixture.may_comment "removed grants nothing" conn ~user:dmem ~post:p_m false in
      let* () = Shared_thread_http_fixture.may_comment "destination ban closes that path" conn ~user:dban ~post false in
      let* () = Shared_thread_http_fixture.add_member conn ~user:dban ~community:d3 in
      let* () = Shared_thread_http_fixture.may_comment "one clean accepted path suffices" conn ~user:dban ~post true in
      let* () = exec conn "ban dban in d3" Shared_thread_http_fixture.q_ban_community (dban, d3) in
      let* () = Shared_thread_http_fixture.may_comment "banned in every path again" conn ~user:dban ~post false in
      let* () = Shared_thread_http_fixture.may_comment "origin ban blocks every path" conn ~user:oban ~post false in
      let* () = Shared_thread_http_fixture.may_comment "global ban blocks every path" conn ~user:gban ~post false in
      let* () = Shared_thread_http_fixture.may_comment "no membership anywhere" conn ~user:stranger ~post false in
      let* () = exec conn "private origin" Community_fixture.q_make_private o in
      let* () = Shared_thread_http_fixture.may_comment "private origin voids destination paths" conn ~user:dmem ~post false in
      let* () = Shared_thread_http_fixture.may_comment "the origin member keeps their own path" conn ~user:author ~post true in
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      let* () = Shared_thread_fixture.disconnect conn ~actor:author ~connection ~acting:o in
      let* () = Shared_thread_http_fixture.may_comment "disconnection keeps participation" conn ~user:dmem ~post true in
      let* () = Shared_thread_fixture.tombstone conn post "[deleted]" in
      let* () = Shared_thread_http_fixture.may_comment "tombstone refuses everyone" conn ~user:author ~post false in
      let* () = exec conn "restore content" Shared_thread_http_fixture.q_restore_content post in
      (* The POST enforces the same rule and preserves context. *)
      let dest_path =
        Earde.Components.canonical_thread_path "sth-dp-d" post "Sth dp thread"
      in
      let post_comment label uid ~content ~context =
        let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting label ~url ~uid () in
        let fields =
          [ ("dream.csrf", token); ("content", content);
            ("post_id", string_of_int post) ]
          @ (match context with
             | Some slug -> [ ("context_community", slug) ]
             | None -> [])
        in
        Shared_thread_http_fixture.send_post ~cookie ~target:"/comments" ~fields pipeline
      in
      let* response, _ =
        post_comment "dmem" dmem ~content:"DPCTX reply"
          ~context:(Some "sth-dp-d")
      in
      Shared_thread_http_fixture.check_location "readable context is preserved" dest_path response;
      (* The comment is the one canonical tree, visible in every context,
         and karma stays canonically origin-scoped. *)
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* _, body =
        Shared_thread_http_fixture.get
          ~target:
            (Earde.Components.canonical_thread_path "sth-dp-o" post
               "Sth dp thread")
          anon
      in
      must body "DPCTX reply";
      let* _, body = Shared_thread_http_fixture.get ~target:dest_path anon in
      must body "DPCTX reply";
      let* n = find conn "origin karma" Shared_thread_http_fixture.q_local_comment_count (dmem, o) in
      Alcotest.(check int) "comment counts toward the ORIGIN stats" 1 n;
      let* n = find conn "dest karma" Shared_thread_http_fixture.q_local_comment_count (dmem, d) in
      Alcotest.(check int) "no destination-local counter" 0 n;
      (* Refusals: participation, origin ban, global ban — each keeps its
         observable shape and writes nothing. *)
      let refused label uid ~status =
        let* response, _ =
          post_comment label uid ~content:"DPCTX refused" ~context:None
        in
        Alcotest.(check int) (label ^ ": status") status (status_of response);
        Lwt.return_unit
      in
      let* () = refused "manual POST without membership" stranger ~status:403 in
      let* () = refused "pending-only member" pmem ~status:403 in
      let* () = refused "origin-banned" oban ~status:403 in
      let* () = refused "globally banned" gban ~status:403 in
      let* n = find conn "no writes" Shared_thread_http_fixture.q_comment_count post in
      Alcotest.(check int) "refusals wrote nothing" 1 n;
      (* A private accepted destination: its member participates and keeps
         the context; a non-member's forged context field decides nothing
         — the comment lands, the redirect falls back to canonical. *)
      let* () = exec conn "private d3" Community_fixture.q_make_private d3 in
      let* d3mem = insert_user conn "sth_dp_d3mem" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:d3mem ~community:d3 in
      let* () = Shared_thread_http_fixture.may_comment "private accepted destination member" conn ~user:d3mem ~post true in
      let* response, _ =
        post_comment "d3mem" d3mem ~content:"DPCTX private"
          ~context:(Some "sth-dp-d3")
      in
      Shared_thread_http_fixture.check_location "authorized private context preserved"
        (Earde.Components.canonical_thread_path "sth-dp-d3" post
           "Sth dp thread")
        response;
      let* response, _ =
        post_comment "dmem forged" dmem ~content:"DPCTX forged"
          ~context:(Some "sth-dp-d3")
      in
      Shared_thread_http_fixture.check_location "forged context falls back"
        (Printf.sprintf "/p/%d" post) response;
      (* A context that stopped being readable falls back too. *)
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:pl ~acting:d in
      let* response, _ =
        post_comment "author stale" author ~content:"DPCTX stale"
          ~context:(Some "sth-dp-d")
      in
      Shared_thread_http_fixture.check_location "stale context falls back"
        (Printf.sprintf "/p/%d" post) response;
      (* Tombstone still refuses the write outright. *)
      let* () = Shared_thread_fixture.tombstone conn post "[deleted]" in
      let* response, _ =
        post_comment "author tombstone" author ~content:"DPCTX tomb"
          ~context:None
      in
      Alcotest.(check int) "tombstoned thread refuses comments" 403
        (status_of response);
      Lwt.return_unit)

(* --- moderation boundary --- *)

let moderation_boundary_case =
  Shared_thread_http_fixture.db_case "destination standing grants no canonical-content moderation: no \
           controls render for a destination top mod, direct mutations are \
           refused, reports stay origin-scoped, and placement removal \
           touches only the placement" (fun ~url conn ->
      let* author, otop, dtop, _o, d, post, _ = Shared_thread_http_fixture.fixture conn "dm" in
      let* cid = find conn "comment" Shared_thread_fixture.q_insert_comment (post, author) in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      let dest_path =
        Earde.Components.canonical_thread_path "sth-dm-d" post "Sth dm thread"
      in
      let canonical =
        Earde.Components.canonical_thread_path "sth-dm-o" post "Sth dm thread"
      in
      (* The destination top mod sees a plain reader's page: no post or
         comment removal, no bans, no management links — and the report
         affordance resolves to the ORIGIN's reporting context. *)
      let* response, body =
        Shared_thread_http_fixture.get ~target:dest_path
          (Shared_thread_http_fixture.session ~url ~uid:dtop ~username:"sth_dm_dtop" ())
      in
      Alcotest.(check int) "dtop reads 200" 200 (status_of response);
      must_not body "Mod Remove";
      must_not body "Mod Ban";
      must_not body "Admin Remove";
      must_not body "/settings/shared-threads";
      must body "href='/c/sth-dm-o/report?type=post";
      must body "/c/sth-dm-o/report?type=comment";
      (* Origin authority travels with the canonical content: the origin
         top mod keeps the controls on the destination-context page. *)
      let* _, body =
        Shared_thread_http_fixture.get ~target:dest_path
          (Shared_thread_http_fixture.session ~url ~uid:otop ~username:"sth_dm_otop" ())
      in
      must body "Mod Remove";
      (* And the destination feed card carries no destination-mod menu. *)
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/c/sth-dm-d"
          (Shared_thread_http_fixture.session ~url ~uid:dtop ~username:"sth_dm_dtop" ())
      in
      must_not body "Mod Remove";
      must_not body "Mod Ban";
      (* Direct mutations under either slug are refused and write
         nothing. *)
      let refuse label ~target ~uid =
        let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting label ~url ~uid () in
        let* response, _ =
          Shared_thread_http_fixture.send_post ~cookie ~target
            ~fields:[ ("dream.csrf", token); ("reason", "dm boundary") ]
            pipeline
        in
        Alcotest.(check bool) (label ^ ": refused") true
          (status_of response >= 400);
        Lwt.return_unit
      in
      let* () =
        refuse "post delete via destination slug"
          ~target:(Printf.sprintf "/c/sth-dm-d/posts/%d/mod_delete" post)
          ~uid:dtop
      in
      let* () =
        refuse "post delete via origin slug"
          ~target:(Printf.sprintf "/c/sth-dm-o/posts/%d/mod_delete" post)
          ~uid:dtop
      in
      let* () =
        refuse "comment delete via destination slug"
          ~target:(Printf.sprintf "/c/sth-dm-d/comments/%d/mod_delete" cid)
          ~uid:dtop
      in
      let* content = find conn "content" Shared_thread_http_fixture.q_post_content post in
      Alcotest.(check (option string)) "canonical post intact"
        (Some "sth body") content;
      let* n = find conn "comments" Shared_thread_http_fixture.q_comment_count post in
      Alcotest.(check int) "canonical comment intact" 1 n;
      (* Placement removal ends the destination rendering and nothing
         else. *)
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:pl ~acting:d in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* response, _ = Shared_thread_http_fixture.get ~target:dest_path anon in
      Shared_thread_http_fixture.check_301 "removed context converges on canonical" canonical response;
      let* response, body = Shared_thread_http_fixture.get ~target:canonical anon in
      Alcotest.(check int) "origin survives removal" 200 (status_of response);
      must body "kept";
      Lwt.return_unit)

(* --- deferred surfaces and public privacy --- *)

let boundary_case =
  Shared_thread_http_fixture.db_case "the deferred discovery surfaces are unchanged — global feed, \
           search, personalized feed, profiles, the legacy redirect — and \
           no private workflow data reaches any public surface"
    (fun ~url conn ->
      let* author, otop, dtop, _o, d, post, _ = Shared_thread_http_fixture.fixture conn "dx" in
      ignore (author, d);
      let* pl =
        Shared_thread_http_fixture.seed_request conn ~actor:otop ~note:"STH_NOTE_DX" ~post
          ~destination:d ()
      in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      let canonical =
        Earde.Components.canonical_thread_path "sth-dx-o" post "Sth dx thread"
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      (* Global feed: the canonical row once, no provenance grammar, no
         destination-context link. *)
      let* response, body = Shared_thread_http_fixture.get ~target:"/feed?scope=all&sort=new" anon in
      Alcotest.(check int) "global feed 200" 200 (status_of response);
      Shared_thread_http_fixture.count "global feed carries the canonical row once" body
        "Sth dx thread" 1;
      must_not body "Shared from";
      must_not body "/c/sth-dx-d/t/";
      (* Personalized feed: unchanged membership surface. *)
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/feed?scope=following&sort=new"
          (Shared_thread_http_fixture.session ~url ~uid:author ())
      in
      Shared_thread_http_fixture.count "personalized feed unchanged" body "Sth dx thread" 1;
      must_not body "Shared from";
      (* Search and the author profile: one canonical result each. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/search?q=Sth%20dx" anon in
      Shared_thread_http_fixture.count "search unchanged" body "Sth dx thread" 1;
      must_not body "Shared from";
      let* _, body = Shared_thread_http_fixture.get ~target:"/u/sth_dx_author" anon in
      Shared_thread_http_fixture.count "profile unchanged" body "Sth dx thread" 1;
      must_not body "Shared from";
      (* The legacy post route still answers the one canonical path. *)
      let* response, _ = Shared_thread_http_fixture.get ~target:(Printf.sprintf "/p/%d" post) anon in
      Shared_thread_http_fixture.check_301 "legacy redirect unchanged" canonical response;
      (* Public privacy: the destination surfaces name no note, no
         requester, no placement id, no internal vocabulary — while the
         canonical author attribution stays. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-dx-d" anon in
      must body "sth_dx_author";
      must_not body "STH_NOTE_DX";
      must_not body "sth_dx_otop";
      must_not body "placement";
      must_not body "shared_thread";
      let* _, body =
        Shared_thread_http_fixture.get
          ~target:
            (Earde.Components.canonical_thread_path "sth-dx-d" post
               "Sth dx thread")
          anon
      in
      must_not body "STH_NOTE_DX";
      must_not body "sth_dx_otop";
      must_not body "placement";
      Lwt.return_unit)

let destination_feed_suite = [ feed_lifecycle_case; feed_ordering_case ]

let destination_section_suite = [ section_stats_case; recent_knowledge_case ]

let destination_context_suite = [ context_url_case ]

let participation_suite = [ comment_participation_case ]

let moderation_suite = [ moderation_boundary_case ]

let boundary_suite = [ boundary_case ]

let suites =
  [ ("shared_thread_destination_feeds", destination_feed_suite)
  ; ("shared_thread_destination_sections", destination_section_suite)
  ; ("shared_thread_destination_thread_context",
     destination_context_suite)
  ; ("shared_thread_comment_participation", participation_suite)
  ; ("shared_thread_moderation_boundary", moderation_suite)
  ; ("shared_thread_read_side_boundary", boundary_suite)
  ]
