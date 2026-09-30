(* Create Shared Thread from the composer: candidates, rendering, the
   canonical post surviving partial failure, pre-insert refusals,
   idempotence, and the origin-side "Shared with" indicator. *)

module AnT = Earde.Analytics.For_testing
let ( let* ) = Lwt.bind
open Caqti_request.Infix

let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let status_of = Http_fixture.status_of
let must = Html_assert.must
let must_not = Html_assert.must_not

(* === Slice 4: Create Shared Thread from the durable composer ===
   The composer converges on the one existing placement operation
   (Shared_thread_placement_store.request): these cases prove the optional
   share area, the candidate read model's composer shape, the
   post-first/placement-second ordering, the fixed notices, and that no
   failure mode can roll back or duplicate the canonical post. *)

module Read = Earde.Shared_thread_placement_read_model

let q_posts_by_title =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM posts WHERE title = $1"

let q_post_id_by_title =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT id FROM posts WHERE title = $1"

let q_local_post_count =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "SELECT COALESCE((SELECT local_post_count FROM community_user_stats \
                    WHERE user_id = $1 AND community_id = $2), 0)::int"

let q_audit_count_for_post =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM shared_thread_placement_audit_events \
   WHERE post_id = $1"

let q_mention_count =
  (Caqti_type.int ->! Caqti_type.int)
  "SELECT COUNT(*)::int FROM notifications \
   WHERE user_id = $1 AND notif_type = 'mention'"

let q_placement_of_post =
  (Caqti_type.int
   ->! Caqti_type.(t2 (t2 string int) (t2 (option string) (option int))))
  "SELECT status, destination_community_id, request_note, \
          requested_by_user_id \
   FROM shared_thread_placements WHERE post_id = $1"

let q_placement_ids_for_post =
  (Caqti_type.int ->* Caqti_type.int64)
  "SELECT id FROM shared_thread_placements WHERE post_id = $1 ORDER BY id"

let q_placements_per_title =
  (Caqti_type.string ->* Caqti_type.int)
  "SELECT (SELECT COUNT(*)::int FROM shared_thread_placements sp \
           WHERE sp.post_id = p.id) \
   FROM posts p WHERE p.title = $1 ORDER BY p.id"

let q_first_post_by_title =
  (Caqti_type.string ->! Caqti_type.int)
  "SELECT MIN(id)::int FROM posts WHERE title = $1"

(* Raw connection fixtures for the read-model exclusion matrix and the
   bound: the status-shape CHECK needs reviewed_at/removed_at coherent
   with the status. The included (accepted) standing still goes through
   the real connections store elsewhere. *)
let q_insert_connection_status =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
  "INSERT INTO community_connections \
     (requester_community_id, recipient_community_id, status, \
      reviewed_at, removed_at) \
   VALUES ($1, $2, $3, \
           CASE WHEN $3 = 'pending' THEN NULL ELSE NOW() END, \
           CASE WHEN $3 = 'removed' THEN NOW() ELSE NULL END)"

let destinations label conn ~origin =
  let* r = Read.connected_destinations conn ~origin_community_id:origin in
  match r with
  | Ok cs -> Lwt.return (List.map Read.candidate_slug cs)
  | Error _ -> Alcotest.failf "%s: storage error" label

let composer_fields ~token ~community ?(title = "t") ?(content = "")
    ?section ?destination ?note () =
  [ ("dream.csrf", token); ("title", title); ("content", content);
    ("community_id", string_of_int community) ]
  @ (match section with
     | Some s -> [ ("section_id", string_of_int s) ]
     | None -> [])
  @ (match destination with
     | Some d -> [ ("share_destination", d) ]
     | None -> [])
  @ (match note with Some n -> [ ("share_note", n) ] | None -> [])

let with_analytics f =
  let payloads = ref [] in
  AnT.use_enabled_test_configuration ();
  AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
  Lwt.finalize
    (fun () -> f payloads)
    (fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ();
      Lwt.return_unit)

(* --- the pure note rule, exactly the domain canonicalizer --- *)

let note_case name input expected =
  Alcotest.test_case name `Quick (fun () ->
      match
        (Earde.Shared_thread_placements.canonical_request_note input, expected)
      with
      | Ok got, Ok want -> Alcotest.(check (option string)) name want got
      | Error _, Error () -> ()
      | Ok _, Error () -> Alcotest.failf "%s: expected refusal" name
      | Error _, Ok _ -> Alcotest.failf "%s: expected acceptance" name)

let composer_note_suite =
  [ note_case "absent stays absent" None (Ok None)
  ; note_case "empty collapses to absent" (Some "") (Ok None)
  ; note_case "whitespace-only collapses" (Some " \t\r\n ") (Ok None)
  ; note_case "CRLF and CR normalize to LF" (Some "a\r\nb\rc")
      (Ok (Some "a\nb\nc"))
  ; note_case "outer ASCII trim only" (Some "  kept  inner\tspacing  ")
      (Ok (Some "kept  inner\tspacing"))
  ; note_case "exactly 2000 scalars pass" (Some (String.make 2000 'a'))
      (Ok (Some (String.make 2000 'a')))
  ; note_case "2001 scalars refuse" (Some (String.make 2001 'a')) (Error ())
  ; note_case "NUL refuses" (Some "a\x00b") (Error ())
  ; note_case "invalid UTF-8 refuses" (Some "a\xffb") (Error ())
  ]

(* --- the composer form, DB-free --- *)

let composer_community : Earde.Community_types.community =
  { id = 8811; slug = "sth-ui-comp";
    name = "Sth Ui Composer";
    description = None; rules = None; avatar_url = None; banner_url = None;
    allow_downvotes = true; sections_enabled = false;
    visibility = Earde.Community_types.Community_public; indexable = true;
    is_network_community = false;
    onboarding_state = Earde.Community_types.Community_published; discoverable = true }

let render_composer ?(share_candidates = []) () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret Github_fixture.cookie_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.new_post_form ~share_candidates [] composer_community
           req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline (Dream.request ~method_:`GET ~target:"/new-post" "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "composer renderer did not run"

let composer_fragment html =
  match Html_assert.index_from html "<div class='create-shell'>" 0 with
  | None -> Alcotest.fail "create shell missing from composer page"
  | Some s -> (
      match Html_assert.index_from html "</main>" s with
      | None -> String.sub html s (String.length html - s)
      | Some e -> String.sub html s (e - s))

let ui_must body needle =
  if not (Html_assert.occurs body ~needle) then
    Alcotest.failf "missing fragment %S" needle

let ui_must_not body needle =
  if Html_assert.occurs body ~needle then
    Alcotest.failf "forbidden fragment %S" needle

let ui_at body needle =
  match Html_assert.index_from body needle 0 with
  | Some i -> i
  | None -> Alcotest.failf "the composer has no %S" needle

let composer_form_absent_case =
  Alcotest.test_case
    "no eligible destination: the composer is the plain pre-slice form"
    `Quick (fun () ->
      let body = composer_fragment (render_composer ()) in
      ui_must_not body "share_destination";
      ui_must_not body "share_note";
      ui_must_not body "Share with a connected community";
      (* The classic fields and the one submit action are intact. *)
      ui_must body "name='title'";
      ui_must body "name='url'";
      ui_must body "name='image'";
      ui_must body "name='content'";
      ui_must body "name='community_id' value='8811'";
      ui_must body ">Submit post</button>")

let composer_form_area_case =
  Alcotest.test_case
    "candidates render one optional select with the blank Do-not-share \
     default, a private note, and no JavaScript" `Quick (fun () ->
      let body =
        composer_fragment
          (render_composer
             ~share_candidates:
               [ ("sth-ui-a", "Alpha Dest");
                 ("sth-ui-evil", "<b>Evil</b> & Sons");
               ]
             ())
      in
      ui_must body "Share with a connected community";
      ui_must body "<option value='' selected>Do not share</option>";
      ui_must body "<option value='sth-ui-a'>Alpha Dest</option>";
      (* Hostile names escape; the raw bytes never cross. *)
      ui_must body "&lt;b&gt;Evil&lt;/b&gt; &amp; Sons";
      ui_must_not body "<b>Evil</b>";
      (* The note is optional, bounded, and visibly private — and the
         stated visibility rule is the implemented one: the requester and
         the authorized moderators, never any public surface. The old
         "read only by the moderators reviewing the request" copy hid the
         requester and must not come back. *)
      ui_must body "name='share_note'";
      ui_must body "maxlength='2000'";
      ui_must body "Private request note";
      ui_must body
        "Visible only to the requester and authorized moderators. Never \
         shown on the thread, in feeds, or in notifications.";
      ui_must_not body "read only by the moderators";
      (* Subordinate placement: after the content field, before the one
         unchanged submit action. *)
      let content_field = ui_at body "name='content'" in
      let share = ui_at body "name='share_destination'" in
      let submit = ui_at body ">Submit post</button>" in
      Alcotest.(check bool) "share area after the text field" true
        (content_field < share);
      Alcotest.(check bool) "share area before the submit action" true
        (share < submit);
      (* Keyboard-accessible labels; no scripts, no inline handlers. *)
      ui_must body "for='share-destination'";
      ui_must body "for='share-note'";
      ui_must_not body "<script";
      ui_must_not body "onclick";
      ui_must_not body "onchange";
      ui_must_not body "onsubmit")

let composer_form_suite = [ composer_form_absent_case; composer_form_area_case ]

(* --- the candidate read model, composer shape --- *)

let candidate_query_case =
  Shared_thread_http_fixture.db_case "connected_destinations: accepted+eligible only, either request \
           direction once, deterministic order, bound applied after \
           eligibility filtering" (fun ~url:_ conn ->
      let* actor = insert_user conn "sth_ca_actor" in
      let* o = Shared_thread_http_fixture.insert_community ~name:"Sth ca Origin" conn "sth-ca-o" in
      let* beta = Shared_thread_http_fixture.insert_community ~name:"beta Dest" conn "sth-ca-beta" in
      let* alpha = Shared_thread_http_fixture.insert_community ~name:"Alpha Dest" conn "sth-ca-alpha" in
      (* One accepted standing per pair, created from either side. *)
      let* _ = Shared_thread_fixture.connect conn ~actor o beta in
      let* _ = Shared_thread_fixture.connect conn ~actor alpha o in
      (* Excluded: every non-accepted connection state... *)
      let* pend = Shared_thread_http_fixture.insert_community ~name:"Aa Pending" conn "sth-ca-pend" in
      let* () =
        exec conn "pending" q_insert_connection_status (o, pend, "pending")
      in
      let* rej = Shared_thread_http_fixture.insert_community ~name:"Aa Rejected" conn "sth-ca-rej" in
      let* () =
        exec conn "rejected" q_insert_connection_status (rej, o, "rejected")
      in
      let* rem = Shared_thread_http_fixture.insert_community ~name:"Aa Removed" conn "sth-ca-rem" in
      let* () =
        exec conn "removed" q_insert_connection_status (o, rem, "removed")
      in
      (* ...and every accepted-but-ineligible destination (legacy
         communities, so lifecycle drift is free of the network CHECKs). *)
      let* priv =
        Community_fixture.insert_community ~network:false ~name:"Aa Private"
          ~visibility:"private" conn "sth-ca-priv"
      in
      let* () =
        exec conn "priv acc" q_insert_connection_status (o, priv, "accepted")
      in
      let* draft =
        Community_fixture.insert_community ~network:false ~name:"Aa Draft"
          ~onboarding:"draft" ~visibility:"private" ~indexable:false
          ~discoverable:false conn "sth-ca-draft"
      in
      let* () =
        exec conn "draft acc" q_insert_connection_status (o, draft, "accepted")
      in
      let* hidden =
        Community_fixture.insert_community ~network:false ~name:"Aa Hidden"
          ~discoverable:false conn "sth-ca-hidden"
      in
      let* () =
        exec conn "hidden acc"
          q_insert_connection_status (o, hidden, "accepted")
      in
      let* slugs = destinations "matrix" conn ~origin:o in
      Alcotest.(check (list string)) "exactly the eligible accepted pair, \
                                      LOWER(name) order"
        [ "sth-ca-alpha"; "sth-ca-beta" ] slugs;
      (* Symmetry: from alpha's side the origin is a candidate and alpha
         itself never is. *)
      let* slugs = destinations "reverse" conn ~origin:alpha in
      Alcotest.(check (list string)) "the counterpart, never itself"
        [ "sth-ca-o" ] slugs;
      (* The bound applies to ELIGIBLE rows: with 57 eligible accepted
         destinations the list holds exactly 50, still ordered, and no
         ineligible name buys a slot. *)
      let* () =
        Lwt_list.iter_s
          (fun i ->
            let slug = Printf.sprintf "sth-ca-b%02d" i in
            let name = Printf.sprintf "bulk %02d" i in
            let* c = Shared_thread_http_fixture.insert_community ~name conn slug in
            exec conn "bulk acc" q_insert_connection_status (o, c, "accepted"))
          (List.init 55 (fun i -> i + 1))
      in
      let* slugs = destinations "bound" conn ~origin:o in
      Alcotest.(check int) "bounded to 50" 50 (List.length slugs);
      (match slugs with
       | first :: _ ->
           Alcotest.(check string) "still name-ordered from the top"
             "sth-ca-alpha" first
       | [] -> Alcotest.fail "empty bounded list");
      Alcotest.(check (option string)) "last eligible inside the bound"
        (Some "sth-ca-b48")
        (List.nth_opt slugs 49);
      Alcotest.(check bool) "no ineligible slot" false
        (List.exists
           (fun s ->
             List.mem s
               [ "sth-ca-priv"; "sth-ca-draft"; "sth-ca-hidden";
                 "sth-ca-pend"; "sth-ca-rej"; "sth-ca-rem"; "sth-ca-o" ])
           slugs);
      Lwt.return_unit)

(* --- the composer page over the real handler --- *)

let composer_page_case =
  Shared_thread_http_fixture.db_case "GET /new-post: the share area lists exactly the server-resolved \
           eligible destinations for the resolved origin, and only for a \
           member" (fun ~url conn ->
      let* author, _otop, _dtop, o, _d, _post, _ = Shared_thread_http_fixture.fixture conn "cb" in
      let* d2 = Shared_thread_http_fixture.insert_community ~name:"Sth cb Second" conn "sth-cb-d2" in
      let* _ = Shared_thread_fixture.connect conn ~actor:author o d2 in
      let* priv =
        Community_fixture.insert_community ~network:false ~name:"Sth cb Private"
          ~visibility:"private" conn "sth-cb-priv"
      in
      let* () =
        exec conn "priv acc" q_insert_connection_status (o, priv, "accepted")
      in
      let author_pipeline = Shared_thread_http_fixture.session ~url ~uid:author () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/new-post?community=sth-cb-o" author_pipeline
      in
      must body "Share with a connected community";
      must body "<option value='' selected>Do not share</option>";
      must body "<option value='sth-cb-d'>Sth cb Dest</option>";
      must body "<option value='sth-cb-d2'>Sth cb Second</option>";
      must_not body "sth-cb-priv";
      must_not body "Sth cb Private";
      must body "Private request note";
      (* A member of a community with no eligible destination keeps the
         plain composer. *)
      let* lone = insert_user conn "sth_cb_lone" in
      let* alone = Shared_thread_http_fixture.insert_community ~name:"Sth cb Alone" conn "sth-cb-alone" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:lone ~community:alone in
      let lone_pipeline = Shared_thread_http_fixture.session ~url ~uid:lone () in
      let* _, body =
        Shared_thread_http_fixture.get ~target:"/new-post?community=sth-cb-alone" lone_pipeline
      in
      must_not body "share_destination";
      must_not body "Share with a connected community";
      must body "name='title'";
      Lwt.return_unit)

(* --- normal creation stays share-free --- *)

let normal_creation_case =
  Shared_thread_http_fixture.db_case "no destination selected: one canonical post, the untouched \
           legacy redirect, and zero shared-thread rows" (fun ~url conn ->
      let* author, _otop, dtop, o, _d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "cc" in
      let* () = exec conn "flat origin" Shared_thread_fixture.q_set_sections (o, false) in
      let* pal = insert_user conn "sth_cc_pal" in
      let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting "cc" ~url ~uid:author () in
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts"
          ~fields:
            (composer_fields ~token ~community:o ~title:"Sth cc plain"
               ~content:"hello @sth_cc_pal" ())
          pipeline
      in
      let* post = find conn "created" q_post_id_by_title "Sth cc plain" in
      Shared_thread_http_fixture.check_location "legacy redirect kept"
        (Printf.sprintf "/p/%d" post)
        response;
      let* n = find conn "one post" q_posts_by_title "Sth cc plain" in
      Alcotest.(check int) "exactly one post" 1 n;
      let* n = find conn "no placement" Shared_thread_http_fixture.q_count_for_post post in
      Alcotest.(check int) "no placement row" 0 n;
      let* n = find conn "no audit" q_audit_count_for_post post in
      Alcotest.(check int) "no audit event" 0 n;
      let* n = find conn "no shared notifs" Shared_thread_http_fixture.q_unread_kinds dtop in
      Alcotest.(check int) "no shared-thread notification" 0 n;
      let* n = find conn "counter" q_local_post_count (author, o) in
      Alcotest.(check int) "local post count incremented" 1 n;
      let* n = find conn "mention" q_mention_count pal in
      Alcotest.(check int) "mention fan-out ran" 1 n;
      (* A blank select is byte-for-byte the same path. *)
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts"
          ~fields:
            (composer_fields ~token ~community:o ~title:"Sth cc blank"
               ~destination:"" ~note:"a note nobody asked to send" ())
          pipeline
      in
      let* post2 = find conn "created 2" q_post_id_by_title "Sth cc blank" in
      Shared_thread_http_fixture.check_location "blank share is the normal path"
        (Printf.sprintf "/p/%d" post2)
        response;
      let* n = find conn "no placement 2" Shared_thread_http_fixture.q_count_for_post post2 in
      Alcotest.(check int) "blank select never shares" 0 n;
      Lwt.return_unit)

(* --- successful Create Shared Thread --- *)

let composer_share_success_case =
  Shared_thread_http_fixture.db_case "a selected destination creates the canonical post first, then \
           exactly the existing pending request with its audit event, \
           notifications, analytics, and both queues" (fun ~url conn ->
      let* author, otop, dtop, o, d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "cd" in
      let* () = exec conn "flat origin" Shared_thread_fixture.q_set_sections (o, false) in
      with_analytics (fun payloads ->
          let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting "cd" ~url ~uid:author () in
          let* response, _ =
            Shared_thread_http_fixture.send_multipart ~cookie ~consent:true ~target:"/posts"
              ~fields:
                (composer_fields ~token ~community:o ~title:"Sth cd shared"
                   ~content:"shared body" ~destination:"sth-cd-d"
                   ~note:"  first line\r\nsecond line  " ())
              pipeline
          in
          let* post = find conn "created" q_post_id_by_title "Sth cd shared" in
          let canonical =
            Earde.Components.canonical_thread_path "sth-cd-o" post
              "Sth cd shared"
          in
          Shared_thread_http_fixture.check_location "PRG to the canonical origin thread"
            (canonical ^ "?shared=requested")
            response;
          (* Exactly one pending placement, origin derived from the post,
             the note canonicalized by the one domain rule. *)
          let* n = find conn "one placement" Shared_thread_http_fixture.q_count_for_post post in
          Alcotest.(check int) "exactly one placement" 1 n;
          let* (status, destination), (note, requested_by) =
            find conn "placement row" q_placement_of_post post
          in
          Alcotest.(check string) "pending" "pending" status;
          Alcotest.(check int) "destination bound" d destination;
          Alcotest.(check (option string)) "canonical note"
            (Some "first line\nsecond line") note;
          Alcotest.(check (option int)) "requested by the author"
            (Some author) requested_by;
          let* n = find conn "audit" q_audit_count_for_post post in
          Alcotest.(check int) "exactly one audit event" 1 n;
          (* Destination top mods are notified; the acting author is not. *)
          let* n = find conn "dtop notified" Shared_thread_http_fixture.q_unread_kinds dtop in
          Alcotest.(check int) "destination top mod notified" 1 n;
          let* n = find conn "author quiet" Shared_thread_http_fixture.q_unread_kinds author in
          Alcotest.(check int) "actor excluded" 0 n;
          (* Analytics: creation event plus exactly one committed-request
             event, identifier-poor and origin-scoped. *)
          let events = List.rev_map Analytics_fixture.event_of !payloads in
          Alcotest.(check (list string)) "events in order"
            [ "forum_thread_created"; "shared_thread_request_submitted" ]
            events;
          (match
             List.find_opt
               (fun p -> Analytics_fixture.event_of p = "shared_thread_request_submitted")
               !payloads
           with
           | None -> Alcotest.fail "missing shared event payload"
           | Some p ->
               Alcotest.(check (slist string compare)) "closed props"
                 [ "user_id"; "community_id"; "post_id"; "$groups";
                   "deployment_environment" ]
                 (Analytics_fixture.prop_keys p);
               Alcotest.(check (option string)) "origin group"
                 (Some ("community:" ^ string_of_int o))
                 (Analytics_fixture.an_group_key p));
          (* Origin renders it immediately; the destination must not until
             the existing review accepts it. *)
          let anon = Shared_thread_http_fixture.app_pipeline ~url () in
          let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-cd-o" anon in
          must body "Sth cd shared";
          let* _, body = Shared_thread_http_fixture.get ~target:"/c/sth-cd-d" anon in
          must_not body "Sth cd shared";
          (* The fixed success notice: only for the closed value, only on
             the origin rendering. *)
          let* _, body = Shared_thread_http_fixture.get ~target:(canonical ^ "?shared=requested") anon in
          must body "Thread created and sharing requested.";
          let* _, body = Shared_thread_http_fixture.get ~target:canonical anon in
          must_not body "Thread created and sharing requested.";
          let* _, body = Shared_thread_http_fixture.get ~target:(canonical ^ "?shared=bogus") anon in
          must_not body "Thread created and sharing requested.";
          must_not body "sharing request could not be sent";
          (* The Share page shows the pending placement and stops offering
             the destination. *)
          let author_pipeline = Shared_thread_http_fixture.session ~url ~uid:author () in
          let* _, body =
            Shared_thread_http_fixture.get ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-cd-o" ~post) author_pipeline
          in
          must body "Sth cd Dest";
          must_not body "<option value='sth-cd-d'>";
          (* Both management queues carry the request through the existing
             review surfaces. *)
          let dtop_pipeline = Shared_thread_http_fixture.session ~url ~uid:dtop () in
          let* _, body = Shared_thread_http_fixture.get ~target:(Shared_thread_http_fixture.mgmt_path "sth-cd-d") dtop_pipeline in
          must body "Sth cd shared";
          let otop_pipeline = Shared_thread_http_fixture.session ~url ~uid:otop () in
          let* _, body = Shared_thread_http_fixture.get ~target:(Shared_thread_http_fixture.mgmt_path "sth-cd-o") otop_pipeline in
          must body "Sth cd shared";
          Lwt.return_unit))

let composer_share_sectioned_case =
  Shared_thread_http_fixture.db_case "a sectioned origin binds the origin forum section exactly as a \
           plain creation; the destination-context page never renders the \
           creation notice" (fun ~url conn ->
      let* author, _otop, dtop, o, d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "ce" in
      let* sid =
        find conn "origin section" Shared_thread_fixture.q_insert_section (o, "notes")
      in
      let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting "ce" ~url ~uid:author () in
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts"
          ~fields:
            (composer_fields ~token ~community:o ~title:"Sth ce sectioned"
               ~section:sid ~destination:"sth-ce-d" ())
          pipeline
      in
      let* post = find conn "created" q_post_id_by_title "Sth ce sectioned" in
      let canonical =
        Earde.Components.canonical_thread_path "sth-ce-o" post
          "Sth ce sectioned"
      in
      Shared_thread_http_fixture.check_location "sectioned PRG" (canonical ^ "?shared=requested")
        response;
      let* stored = find conn "section" Shared_thread_http_fixture.q_origin_section_of post in
      Alcotest.(check (option int)) "origin section bound" (Some sid) stored;
      let* n = find conn "one placement" Shared_thread_http_fixture.q_count_for_post post in
      Alcotest.(check int) "one pending placement" 1 n;
      (* Accept it, then prove the destination-context page ignores the
         notice vocabulary. *)
      let* placements =
        Db_fixture.collect conn "ids" q_placement_ids_for_post post
      in
      (match placements with
       | [ placement ] ->
           let* () =
             Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement ~destination:d ()
           in
           let dest =
             Earde.Components.canonical_thread_path "sth-ce-d" post
               "Sth ce sectioned"
           in
           let anon = Shared_thread_http_fixture.app_pipeline ~url () in
           let* _, body = Shared_thread_http_fixture.get ~target:(dest ^ "?shared=requested") anon in
           must_not body "Thread created and sharing requested.";
           must body "Shared from";
           Lwt.return_unit
       | l ->
           Alcotest.failf "expected one placement id, got %d" (List.length l))
      )

(* --- partial success: the canonical post always survives --- *)

let partial_attempt conn ~url ~author ~origin_slug ~community ~pal label
    ~destination ~title =
  let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting label ~url ~uid:author () in
  let* mentions_before = find conn "mentions before" q_mention_count pal in
  let* count_before =
    find conn "counter before" q_local_post_count (author, community)
  in
  let* response, _ =
    Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts"
      ~fields:
        (composer_fields ~token ~community ~title
           ~content:("cp body @sth_cp_pal for " ^ label)
           ~destination ~note:"CP_PRIVATE_NOTE" ())
      pipeline
  in
  let* post = find conn (label ^ ": created") q_post_id_by_title title in
  let canonical =
    Earde.Components.canonical_thread_path origin_slug post title
  in
  Shared_thread_http_fixture.check_location (label ^ ": partial-success PRG")
    (canonical ^ "?shared=failed")
    response;
  let* n = find conn (label ^ ": placements") Shared_thread_http_fixture.q_count_for_post post in
  Alcotest.(check int) (label ^ ": no placement") 0 n;
  let* n = find conn (label ^ ": audit") q_audit_count_for_post post in
  Alcotest.(check int) (label ^ ": no audit") 0 n;
  let* mentions_after = find conn "mentions after" q_mention_count pal in
  Alcotest.(check int) (label ^ ": mention still ran")
    (mentions_before + 1) mentions_after;
  let* count_after =
    find conn "counter after" q_local_post_count (author, community)
  in
  Alcotest.(check int) (label ^ ": local count still ran")
    (count_before + 1) count_after;
  Lwt.return canonical

let composer_partial_case =
  Shared_thread_http_fixture.db_case "every sharing failure keeps the canonical post and its side \
           effects, writes no placement, and lands on the fixed \
           partial-success notice with a safe Share-page link"
    (fun ~url conn ->
      let* author, _otop, dtop, o, _d, _fixture_post, connection =
        Shared_thread_http_fixture.fixture conn "cp"
      in
      let* () = exec conn "flat origin" Shared_thread_fixture.q_set_sections (o, false) in
      let* pal = insert_user conn "sth_cp_pal" in
      let* d2 = Shared_thread_http_fixture.insert_community ~name:"Sth cp Second" conn "sth-cp-d2" in
      let* _ = Shared_thread_fixture.connect conn ~actor:author o d2 in
      let* _unconnected =
        Shared_thread_http_fixture.insert_community ~name:"Sth cp Loose" conn "sth-cp-loose"
      in
      let attempt = partial_attempt conn ~url ~author ~origin_slug:"sth-cp-o"
          ~community:o ~pal
      in
      (* The connection vanished between form render and submit. *)
      let* () =
        Shared_thread_fixture.disconnect conn ~actor:author ~connection ~acting:o
      in
      let* canonical =
        attempt "disconnected" ~destination:"sth-cp-d" ~title:"Sth cp one"
      in
      (* The destination stopped being eligible. *)
      let* () = exec conn "private d2" Community_fixture.q_make_private d2 in
      let* _ =
        attempt "ineligible" ~destination:"sth-cp-d2" ~title:"Sth cp two"
      in
      (* Tampering: an unconnected community, the origin itself, and a
         slug that names nothing. *)
      let* _ =
        attempt "unconnected" ~destination:"sth-cp-loose"
          ~title:"Sth cp three"
      in
      let* _ =
        attempt "origin itself" ~destination:"sth-cp-o" ~title:"Sth cp four"
      in
      let* _ =
        attempt "ghost slug" ~destination:"sth-cp-ghost"
          ~title:"Sth cp five"
      in
      (* No destination top mod heard anything, and the private note is
         nowhere — not in the durable rows, not on any surface. *)
      let* n = find conn "dtop quiet" Shared_thread_http_fixture.q_unread_kinds dtop in
      Alcotest.(check int) "no shared-thread notification" 0 n;
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* _, body = Shared_thread_http_fixture.get ~target:(canonical ^ "?shared=failed") anon in
      must body "Thread created, but the sharing request could not be sent.";
      must body (canonical ^ "/share");
      (* One fixed copy for every cause: neither the note nor any
         destination identity leaks through the notice. *)
      must_not body "CP_PRIVATE_NOTE";
      must_not body "sth-cp-d";
      Lwt.return_unit)

let composer_store_failure_case =
  Shared_thread_http_fixture.db_case "a placement-store failure rolls its whole transaction back, the \
           post survives, no shared event is captured, and the Share page \
           still offers the intact destination for a real retry"
    (fun ~url conn ->
      let* author, _otop, dtop, o, _d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "cf" in
      let* () = exec conn "flat origin" Shared_thread_fixture.q_set_sections (o, false) in
      with_analytics (fun payloads ->
          let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting "cf" ~url ~uid:author () in
          let* () =
            Shared_thread_fixture.with_poison conn ~install:Shared_thread_fixture.q_poison_notif
              ~remove:Shared_thread_fixture.q_unpoison_notif (fun () ->
                let* response, _ =
                  Shared_thread_http_fixture.send_multipart ~cookie ~consent:true ~target:"/posts"
                    ~fields:
                      (composer_fields ~token ~community:o
                         ~title:"Sth cf stormy" ~destination:"sth-cf-d" ())
                    pipeline
                in
                let* post =
                  find conn "created" q_post_id_by_title "Sth cf stormy"
                in
                let canonical =
                  Earde.Components.canonical_thread_path "sth-cf-o" post
                    "Sth cf stormy"
                in
                Shared_thread_http_fixture.check_location "store failure is partial success"
                  (canonical ^ "?shared=failed")
                  response;
                let* n = find conn "placements" Shared_thread_http_fixture.q_count_for_post post in
                Alcotest.(check int) "atomic: no partial placement" 0 n;
                let* n = find conn "audit" q_audit_count_for_post post in
                Alcotest.(check int) "atomic: no orphan audit" 0 n;
                Lwt.return_unit)
          in
          (* Only the creation event: a failed request captures nothing. *)
          Alcotest.(check (list string)) "no shared analytics on failure"
            [ "forum_thread_created" ]
            (List.rev_map Analytics_fixture.event_of !payloads);
          (* The destination is intact, so the Share page still offers it,
             and the existing share flow completes the retry. *)
          let* post = find conn "post" q_post_id_by_title "Sth cf stormy" in
          let author_pipeline = Shared_thread_http_fixture.session ~url ~uid:author () in
          let* _, body =
            Shared_thread_http_fixture.get ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-cf-o" ~post) author_pipeline
          in
          must body "<option value='sth-cf-d'>";
          let* pipeline, cookie, token, _ =
            Shared_thread_http_fixture.acting "cf retry" ~url ~uid:author ()
          in
          let* response, _ =
            Shared_thread_http_fixture.send_post ~cookie
              ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-cf-o" ~post)
              ~fields:
                [ ("dream.csrf", token); ("destination", "sth-cf-d");
                  ("note", "") ]
              pipeline
          in
          Alcotest.(check int) "retry accepted" 303 (status_of response);
          let* n = find conn "retried placement" Shared_thread_http_fixture.q_count_for_post post in
          Alcotest.(check int) "exactly one placement after retry" 1 n;
          let* n = find conn "dtop notified" Shared_thread_http_fixture.q_unread_kinds dtop in
          Alcotest.(check int) "retry notified the destination" 1 n;
          Lwt.return_unit))

(* --- pre-insert refusals: nothing exists afterwards --- *)

let composer_preinsert_case =
  Shared_thread_http_fixture.db_case "CSRF, canonical-field, section, membership, ban, and note \
           failures refuse before any write: no post, no placement, no \
           leaked destination or note" (fun ~url conn ->
      let* author, _otop, _dtop, o, _d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "cg" in
      let* sid =
        find conn "origin section" Shared_thread_fixture.q_insert_section (o, "field")
      in
      let* other = Shared_thread_http_fixture.insert_community ~name:"Sth cg Other" conn "sth-cg-x" in
      let* xsid =
        find conn "foreign section" Shared_thread_fixture.q_insert_section
          (other, "alien")
      in
      let none_created label =
        let* n = find conn (label ^ ": posts") q_posts_by_title "Sth cg nope" in
        Alcotest.(check int) (label ^ ": no post") 0 n;
        Lwt.return_unit
      in
      let* pipeline, cookie, token, expired = Shared_thread_http_fixture.acting "cg" ~url ~uid:author () in
      let refuse label ~fields ~status:expected =
        let* response, body = Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts" ~fields pipeline in
        Alcotest.(check int) (label ^ ": status") expected
          (status_of response);
        must_not body "CG_SECRET_NOTE";
        must_not body "sth-cg-d";
        let* () = none_created label in
        Lwt.return_unit
      in
      let base ?(title = "Sth cg nope") ?(token = token) ?section
          ?(destination = "sth-cg-d") ?(note = "CG_SECRET_NOTE") () =
        composer_fields ~token ~community:o ~title ?section ~destination
          ~note ()
      in
      let* () =
        refuse "stale CSRF" ~fields:(base ~token:expired ~section:sid ())
          ~status:200
      in
      let* () =
        refuse "empty title" ~fields:(base ~title:"" ~section:sid ())
          ~status:200
      in
      let* () =
        refuse "missing section" ~fields:(base ()) ~status:200
      in
      let* () =
        refuse "foreign section" ~fields:(base ~section:xsid ()) ~status:400
      in
      let* () =
        refuse "overlong note"
          ~fields:
            (composer_fields ~token ~community:o ~title:"Sth cg nope"
               ~section:sid ~destination:"sth-cg-d"
               ~note:(String.make 2001 'a') ())
          ~status:200
      in
      (* Identity gates, each with its own account. *)
      let* stranger = insert_user conn "sth_cg_str" in
      let* spipe, scookie, stoken, _ = Shared_thread_http_fixture.acting "cg str" ~url ~uid:stranger () in
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie:scookie ~target:"/posts"
          ~fields:
            (composer_fields ~token:stoken ~community:o ~title:"Sth cg nope"
               ~section:sid ~destination:"sth-cg-d" ())
          spipe
      in
      Alcotest.(check int) "non-member refused" 200 (status_of response);
      let* () = none_created "non-member" in
      let* gban = insert_user conn "sth_cg_gban" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:gban ~community:o in
      let* () = exec conn "gban" Shared_thread_http_fixture.q_ban_global gban in
      let* gpipe, gcookie, gtoken, _ = Shared_thread_http_fixture.acting "cg gban" ~url ~uid:gban () in
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie:gcookie ~target:"/posts"
          ~fields:
            (composer_fields ~token:gtoken ~community:o ~title:"Sth cg nope"
               ~section:sid ~destination:"sth-cg-d" ())
          gpipe
      in
      Alcotest.(check int) "global ban refused" 403 (status_of response);
      let* () = none_created "global ban" in
      let* oban = insert_user conn "sth_cg_oban" in
      let* () = Shared_thread_http_fixture.add_member conn ~user:oban ~community:o in
      let* () = exec conn "oban" Shared_thread_http_fixture.q_ban_community (oban, o) in
      let* opipe, ocookie, otoken, _ = Shared_thread_http_fixture.acting "cg oban" ~url ~uid:oban () in
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie:ocookie ~target:"/posts"
          ~fields:
            (composer_fields ~token:otoken ~community:o ~title:"Sth cg nope"
               ~section:sid ~destination:"sth-cg-d" ())
          opipe
      in
      Alcotest.(check int) "origin ban refused" 403 (status_of response);
      let* () = none_created "origin ban" in
      (* The overlong note only matters when a destination is selected:
         without one it is ignored and the post is created normally. *)
      let* response, _ =
        Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts"
          ~fields:
            (composer_fields ~token ~community:o ~title:"Sth cg noteless"
               ~section:sid ~note:(String.make 2001 'a') ())
          pipeline
      in
      let* post = find conn "created" q_post_id_by_title "Sth cg noteless" in
      Shared_thread_http_fixture.check_location "ignored note, normal path"
        (Printf.sprintf "/p/%d" post)
        response;
      let* n = find conn "no placement" Shared_thread_http_fixture.q_count_for_post post in
      Alcotest.(check int) "no placement without a destination" 0 n;
      Lwt.return_unit)

(* --- idempotence and double submission --- *)

let composer_idempotence_case =
  Shared_thread_http_fixture.db_case "one POST creates one post and one placement; a repeated \
           submission creates a second distinct post, never a second \
           placement on the first; the share route cannot double-place \
           either" (fun ~url conn ->
      let* author, _otop, _dtop, o, _d, _fixture_post, _ = Shared_thread_http_fixture.fixture conn "ch" in
      let* () = exec conn "flat origin" Shared_thread_fixture.q_set_sections (o, false) in
      let* pipeline, cookie, token, _ = Shared_thread_http_fixture.acting "ch" ~url ~uid:author () in
      let fields =
        composer_fields ~token ~community:o ~title:"Sth ch twice"
          ~destination:"sth-ch-d" ()
      in
      let* _ = Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts" ~fields pipeline in
      let* _ = Shared_thread_http_fixture.send_multipart ~cookie ~target:"/posts" ~fields pipeline in
      let* n = find conn "posts" q_posts_by_title "Sth ch twice" in
      Alcotest.(check int)
        "a repeated browser submission is a second canonical post" 2 n;
      let* per_post =
        Db_fixture.collect conn "per post" q_placements_per_title
          "Sth ch twice"
      in
      Alcotest.(check (list int)) "exactly one placement each" [ 1; 1 ]
        per_post;
      (* The Share page path cannot add a second active placement for the
         same post and destination. *)
      let* post = find conn "first post" q_first_post_by_title "Sth ch twice" in
      let* response, _ =
        Shared_thread_http_fixture.send_post ~cookie
          ~target:(Shared_thread_http_fixture.share_path ~slug:"sth-ch-o" ~post)
          ~fields:
            [ ("dream.csrf", token); ("destination", "sth-ch-d");
              ("note", "") ]
          pipeline
      in
      Alcotest.(check int) "active placement collapses to conflict" 409
        (status_of response);
      let* n = find conn "still one" Shared_thread_http_fixture.q_count_for_post post in
      Alcotest.(check int) "still exactly one placement" 1 n;
      Lwt.return_unit)

(* === Origin-side "Shared with" indicator ===
   The reverse provenance direction: the canonical origin thread page and
   the global-feed canonical card name the currently publicly renderable
   accepted destinations. These cases prove the launch copy, the lifecycle
   and current-visibility filtering, the deterministic multi-destination
   ordering, that the destination context keeps its one "Shared from"
   direction, and that the enrichment cannot touch global-feed selection,
   ordering, or LIMIT/OFFSET. *)

(* The centralized copy helper is pure: exact copy for zero, one, and many
   destinations, and everything escaped. *)
let shared_with_copy_case =
  Alcotest.test_case
    "shared_with_html: exact launch copy and escaping for 0/1/n destinations"
    `Quick (fun () ->
      let render = Earde.Components.shared_with_html in
      Alcotest.(check string) "empty renders nothing" "" (render []);
      Alcotest.(check string) "one destination"
        "<span class='sth-shared-from'>&#8644; Shared with \
         <a href='/c/dest-a'>Dest A</a></span>"
        (render [ ("dest-a", "Dest A") ]);
      Alcotest.(check string) "two destinations"
        "<span class='sth-shared-from'>&#8644; Shared with \
         <a href='/c/dest-a'>Dest A</a> and 1 more</span>"
        (render [ ("dest-a", "Dest A"); ("dest-b", "Dest B") ]);
      Alcotest.(check string) "three destinations count the tail"
        "<span class='sth-shared-from'>&#8644; Shared with \
         <a href='/c/dest-a'>Dest A</a> and 2 more</span>"
        (render
           [ ("dest-a", "Dest A"); ("dest-b", "Dest B"); ("dest-c", "Dest C") ]);
      Alcotest.(check string) "name and slug are HTML-escaped"
        "<span class='sth-shared-from'>&#8644; Shared with \
         <a href='/c/x&#39;y'>Ev&lt;il&gt;&amp;</a></span>"
        (render [ ("x'y", "Ev<il>&") ]))

let origin_indicator_case =
  Shared_thread_http_fixture.db_case "origin surfaces name only currently publicly renderable accepted \
           destinations: lifecycle filtering, deterministic multi-destination \
           copy, private-destination and private-origin hiding, removal, and \
           the destination context keeps its own single direction"
    (fun ~url conn ->
      let* author, otop, dtop, o, d, post, _ = Shared_thread_http_fixture.fixture conn "oi" in
      (* Second flat destination whose lower-cased name sorts FIRST, so the
         deterministic head of the copy is decided by ordering, not by
         acceptance order. *)
      let* d2top = insert_user conn "sth_oi_d2top" in
      let* d2 = Shared_thread_http_fixture.insert_community ~name:"Sth oi Aux" conn "sth-oi-aux" in
      let* () = exec conn "flat d2" Shared_thread_fixture.q_set_sections (d2, false) in
      let* () = Shared_thread_http_fixture.add_top_mod conn ~user:d2top ~community:d2 in
      let* _ = Shared_thread_fixture.connect conn ~actor:otop o d2 in
      (* One post per non-accepted lifecycle state. *)
      let* p_pending =
        find conn "pending post" Shared_thread_http_fixture.q_insert_post ("Sth oi pending", (o, author))
      in
      let* p_rejected =
        find conn "rejected post" Shared_thread_http_fixture.q_insert_post
          ("Sth oi rejected", (o, author))
      in
      let* p_withdrawn =
        find conn "withdrawn post" Shared_thread_http_fixture.q_insert_post
          ("Sth oi withdrawn", (o, author))
      in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      let* pl2 = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d2 () in
      let* () =
        Shared_thread_http_fixture.seed_accept conn ~reviewer:d2top ~placement:pl2 ~destination:d2 ()
      in
      let* _pending =
        Shared_thread_http_fixture.seed_request conn ~actor:author ~note:"STH_NOTE_OI" ~post:p_pending
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
      let canonical =
        Earde.Components.canonical_thread_path "sth-oi-o" post
          "Sth oi thread"
      in
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      (* Origin canonical page: deterministic first destination + count,
         exactly one indicator, no reverse direction, no private note. *)
      let* response, body = Shared_thread_http_fixture.get ~target:canonical anon in
      Alcotest.(check int) "origin thread 200" 200 (status_of response);
      must body
        "Shared with <a href='/c/sth-oi-aux'>Sth oi Aux</a> and 1 more";
      Shared_thread_http_fixture.count "one origin indicator" body "Shared with" 1;
      must_not body "Shared from";
      must_not body "STH_NOTE_OI";
      (* The view model keeps the FULL ordered list (the rendered copy is
         only its head), and only the accepted post produces rows. *)
      let* r =
        Earde.Shared_thread_reading.public_destinations_for_posts conn
          ~post_ids:[ post; p_pending; p_rejected; p_withdrawn ]
      in
      (match r with
       | Ok rows ->
           Alcotest.(check (list (pair int (pair string string))))
             "full deterministic destination list"
             [ (post, ("sth-oi-aux", "Sth oi Aux"));
               (post, ("sth-oi-d", "Sth oi Dest"))
             ]
             rows
       | Error _ -> Alcotest.fail "read model: storage error");
      (* Non-accepted lifecycle states produce no indicator on their own
         origin pages. *)
      let check_absent title p =
        let* _, body =
          Shared_thread_http_fixture.get
            ~target:(Earde.Components.canonical_thread_path "sth-oi-o" p title)
            anon
        in
        must_not body "Shared with";
        Lwt.return_unit
      in
      let* () = check_absent "Sth oi pending" p_pending in
      let* () = check_absent "Sth oi rejected" p_rejected in
      let* () = check_absent "Sth oi withdrawn" p_withdrawn in
      (* Global feed: the one canonical card is enriched — no duplication,
         no destination-context link, no note, and exactly one indicator
         across the whole page. *)
      let* _, body = Shared_thread_http_fixture.get ~target:"/feed?scope=all&sort=new" anon in
      Shared_thread_http_fixture.count "one canonical card" body "Sth oi thread" 1;
      Shared_thread_http_fixture.count "one feed indicator" body "Shared with" 1;
      must body "Sth oi Aux";
      must_not body "STH_NOTE_OI";
      must_not body "/c/sth-oi-d/t/";
      must_not body "/c/sth-oi-aux/t/";
      (* Destination context keeps its one direction. *)
      let* _, body =
        Shared_thread_http_fixture.get
          ~target:
            (Earde.Components.canonical_thread_path "sth-oi-d" post
               "Sth oi thread")
          anon
      in
      must body "Shared from";
      must_not body "Shared with";
      (* A destination that is no longer publicly renderable disappears
         silently: name, slug, and count all go. *)
      let* () = exec conn "privatize aux" Community_fixture.q_make_private d2 in
      let* response, body = Shared_thread_http_fixture.get ~target:canonical anon in
      Alcotest.(check int) "origin thread still 200" 200 (status_of response);
      must body "Shared with <a href='/c/sth-oi-d'>Sth oi Dest</a></span>";
      must_not body "and 1 more";
      must_not body "Sth oi Aux";
      must_not body "sth-oi-aux";
      (* An origin no longer public stops the public naming entirely (the
         destination rendering itself has stopped, so the origin side must
         not keep claiming it). *)
      let* () = exec conn "privatize origin" Community_fixture.q_make_private o in
      let* r =
        Earde.Shared_thread_reading.public_destinations_for_posts conn
          ~post_ids:[ post ]
      in
      (match r with
       | Ok rows ->
           Alcotest.(check (list (pair int (pair string string))))
             "private origin names nothing" [] rows
       | Error _ -> Alcotest.fail "read model: storage error");
      let* () = exec conn "restore origin" Shared_thread_fixture.q_make_eligible o in
      let* () = exec conn "restore aux" Shared_thread_fixture.q_make_eligible d2 in
      (* Removal ends exactly its own indicator entry; removing the last
         accepted placement ends the indicator everywhere. *)
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:d2top ~placement:pl2 ~acting:d2 in
      let* _, body = Shared_thread_http_fixture.get ~target:canonical anon in
      must body "Shared with";
      must body "Sth oi Dest";
      must_not body "Sth oi Aux";
      must_not body "and 1 more";
      let* () = Shared_thread_http_fixture.remove_seed conn ~actor:dtop ~placement:pl ~acting:d in
      let* _, body = Shared_thread_http_fixture.get ~target:canonical anon in
      must_not body "Shared with";
      let* _, body = Shared_thread_http_fixture.get ~target:"/feed?scope=all&sort=new" anon in
      must_not body "Shared with";
      Lwt.return_unit)

let origin_feed_invariance_case =
  Shared_thread_http_fixture.db_case "global-feed selection, ordering, and LIMIT/OFFSET are unchanged \
           by origin-side enrichment: identical ids before and after \
           acceptance, one row per canonical post, relative order intact"
    (fun ~url conn ->
      let* author, _otop, dtop, o, d, post, _ = Shared_thread_http_fixture.fixture conn "ov" in
      let* pa = find conn "pa" Shared_thread_http_fixture.q_insert_post ("Sth ov alpha", (o, author)) in
      let* pb = find conn "pb" Shared_thread_http_fixture.q_insert_post ("Sth ov beta", (o, author)) in
      (* Distinct ages decide sort=new deterministically among the
         fixtures: beta (2h) before shared (3h) before alpha (4h). *)
      let* () = exec conn "age pa" Shared_thread_http_fixture.q_age_post (pa, 4) in
      let* () = exec conn "age shared" Shared_thread_http_fixture.q_age_post (post, 3) in
      let* () = exec conn "age pb" Shared_thread_http_fixture.q_age_post (pb, 2) in
      let ids_of limit offset =
        let* r = Earde.Post_store.get_all_posts conn Earde.Post_types.Newest limit offset in
        Lwt.return
          (List.map
             (fun (p : Earde.Post_types.post) -> p.id)
             (Shared_thread_http_fixture.ok "get_all_posts" r))
      in
      (* The same fixtures WITHOUT enrichment: capture the exact feed
         windows before any placement exists... *)
      let* before_full = ids_of 50 0 in
      let* before_window = ids_of 2 1 in
      let* pl = Shared_thread_http_fixture.seed_request conn ~actor:author ~post ~destination:d () in
      let* () = Shared_thread_http_fixture.seed_accept conn ~reviewer:dtop ~placement:pl ~destination:d () in
      (* ...and prove the accepted placement changed neither the selected
         ids, nor their order, nor what a LIMIT/OFFSET window selects. *)
      let* after_full = ids_of 50 0 in
      let* after_window = ids_of 2 1 in
      Alcotest.(check (list int)) "same ids, same order" before_full
        after_full;
      Alcotest.(check (list int)) "same LIMIT/OFFSET window" before_window
        after_window;
      (* HTTP: each fixture row renders exactly once, in the canonical
         relative order, with the enriched card carrying the indicator. *)
      let anon = Shared_thread_http_fixture.app_pipeline ~url () in
      let* _, body = Shared_thread_http_fixture.get ~target:"/feed?scope=all&sort=new" anon in
      Shared_thread_http_fixture.count "alpha once" body "Sth ov alpha" 1;
      Shared_thread_http_fixture.count "beta once" body "Sth ov beta" 1;
      Shared_thread_http_fixture.count "shared once" body "Sth ov thread" 1;
      Shared_thread_http_fixture.count "one indicator" body "Shared with" 1;
      let idx label =
        match Html_assert.index_from body label 0 with
        | Some i -> i
        | None -> Alcotest.failf "feed row %S missing" label
      in
      Alcotest.(check bool) "canonical relative order intact" true
        (idx "Sth ov beta" < idx "Sth ov thread"
         && idx "Sth ov thread" < idx "Sth ov alpha");
      Lwt.return_unit)

let composer_candidate_suite = [ candidate_query_case ]

let composer_render_suite = [ composer_page_case ]

let composer_creation_suite = [ normal_creation_case ]

let composer_share_suite =
  [ composer_share_success_case; composer_share_sectioned_case ]

let composer_partial_suite =
  [ composer_partial_case; composer_store_failure_case ]

let composer_preinsert_suite = [ composer_preinsert_case ]

let composer_idempotence_suite = [ composer_idempotence_case ]

let origin_indicator_suite =
  [ shared_with_copy_case; origin_indicator_case
  ; origin_feed_invariance_case ]

let suites =
    (* Origin-side "Shared with" indicator: the pure centralized copy
       helper, then database-gated: the origin thread page and canonical
       global-feed card naming only currently publicly renderable accepted
       destinations, and the proof that enrichment leaves global-feed
       selection, ordering, and LIMIT/OFFSET untouched. *)
  [ ("shared_thread_origin_indicator", origin_indicator_suite)
    (* Slice 4 — Create Shared Thread from the durable composer. The pure
       note rule and the DB-free composer form; then, database-gated: the
       composer-shaped candidate read model, the real GET /new-post
       rendering, the untouched share-free creation path, the successful
       composer request (post first, then exactly the existing pending
       placement with audit, notifications, and analytics), the
       partial-success matrix (the canonical post always survives), the
       pre-insert refusals, and double-submission behaviour. *)
  ; ("shared_thread_composer_note_domain", composer_note_suite)
  ; ("shared_thread_composer_form_ui", composer_form_suite)
  ; ("shared_thread_composer_candidates", composer_candidate_suite)
  ; ("shared_thread_composer_rendering", composer_render_suite)
  ; ("shared_thread_composer_normal_creation", composer_creation_suite)
  ; ("shared_thread_composer_request", composer_share_suite)
  ; ("shared_thread_composer_partial_success", composer_partial_suite)
  ; ("shared_thread_composer_preinsert", composer_preinsert_suite)
  ; ("shared_thread_composer_idempotence", composer_idempotence_suite)
  ]
