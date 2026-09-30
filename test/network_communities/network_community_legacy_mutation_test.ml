(* === Legacy mutation bypass closure for network communities ===
   The three existing routes that can move a community's canonical identity
   or its publication/discovery lifecycle — POST /update-community, POST
   /c/:slug/settings/visibility, and POST /c/:slug/settings/indexability —
   driven as the real handlers over real durable rows. Each must fail closed
   *before* any write on a network community rather than becoming a scoped
   CHECK violation rendered back as a database error, and each must leave a
   legacy community's behaviour byte-for-byte unchanged. Database-gated, with
   ncpg_% usernames and ncpg-% community slugs; no project fixtures are
   needed, because only communities.is_network_community and
   communities.onboarding_state drive the guards. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

let exec = Db_fixture.exec

let find = Db_fixture.find

let insert_community = Community_fixture.insert_community

let status_of = Http_fixture.status_of

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM communities WHERE slug LIKE 'ncpg-%'"
    ; "DELETE FROM users WHERE username LIKE 'ncpg_%'"
    ]

(* Everything the guarded routes could move, as one comparable signature:
   if a guard leaks, this string changes. *)
let q_state =
  (Caqti_type.int ->! Caqti_type.string)
  "SELECT name || '|' || COALESCE(description, '<null>') || '|' || \
          visibility || '|' || onboarding_state || '|' || \
          is_network_community::text || '|' || indexable::text || '|' || \
          discoverable::text \
   FROM communities WHERE id = $1"

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

(* One shared single-connection sql_pool for the whole suite (see the
   sibling handler suites for why a per-request pool is not an option).
   Session identity, form fields, and encoding are swapped per request
   through refs; cases run sequentially. The router binds exactly the three
   real guarded handlers. *)
let shared_identity : (int * bool) option ref = ref None

let shared_form : (string * string) list ref = ref []

let shared_multipart = ref false

let shared_pipeline = ref None

let build_pipeline ~url =
  Dream.sql_pool ~size:1 url @@ Dream.set_secret Github_fixture.cookie_secret
  @@ Dream.memory_sessions
  @@ (fun handler request ->
       let* () =
         match !shared_identity with
         | None -> Lwt.return_unit
         | Some (uid, is_admin) ->
             let* () =
               Dream.set_session_field request "user_id" (string_of_int uid)
             in
             let* () =
               Dream.set_session_field request "username"
                 ("ncpg_user_" ^ string_of_int uid)
             in
             if is_admin then Dream.set_session_field request "is_admin" "true"
             else Lwt.return_unit
       in
       let csrf = Dream.csrf_token request in
       let fields = ("dream.csrf", csrf) :: !shared_form in
       Dream.set_body request
         (if !shared_multipart then Http_fixture.multipart_body fields
          else Http_fixture.encoded_form_body fields);
       handler request)
  @@ Dream.router
       [ Dream.post "/update-community" Earde.Community_settings_handlers.update_community_handler;
         Dream.post "/c/:slug/settings/visibility"
           Earde.Community_settings_handlers.update_community_visibility_handler;
         Dream.post "/c/:slug/settings/indexability"
           Earde.Community_settings_handlers.update_community_indexability_handler
       ]

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline = build_pipeline ~url in
      shared_pipeline := Some pipeline;
      pipeline

let post ~url ~target ?(multipart = false) ~user ?(admin_session = false)
    fields =
  shared_identity := Some (user, admin_session);
  shared_form := fields;
  shared_multipart := multipart;
  let headers =
    [ ( "Content-Type",
        if multipart then
          "multipart/form-data; boundary=" ^ Http_fixture.multipart_boundary
        else "application/x-www-form-urlencoded" ) ]
  in
  let* response =
    (pipeline_for ~url) (Dream.request ~method_:`POST ~target ~headers "")
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

(* The multipart detail form, plus the two legacy existing_* fallback
   fields the settings page no longer emits. They are supplied here by
   hand, empty; the handler ignores them either way. *)
let detail_fields ~community_id ~community_slug ~description =
  [ ("community_id", string_of_int community_id);
    ("community_slug", community_slug);
    ("description", description);
    ("rules", "");
    ("avatar_url", "");
    ("banner_url", "");
    ("existing_avatar_url", "");
    ("existing_banner_url", "")
  ]

let state conn id = find conn "state" q_state id

let unchanged label conn id before =
  let* after = state conn id in
  Alcotest.(check string) (label ^ ": durable state unchanged") before after;
  Lwt.return_unit

(* A guarded response must be generic: no lifecycle, no authorization
   detail, no constraint name, no SQL. *)
let check_opaque label body =
  List.iter
    (fun needle ->
      Alcotest.(check bool) (label ^ ": no " ^ needle) false
        (Html_assert.contains body needle))
    [ "communities_network"; "constraint"; "char_length"; "Caqti";
      "PostgreSQL"; "Database error"; "onboarding_state";
      "is_network_community"; "top_mod"
    ]

(* === fixtures === *)

let draft conn slug =
  insert_community ~visibility:"private" ~indexable:false ~network:true
    ~onboarding:"draft" ~discoverable:false conn slug

let published_network conn slug =
  insert_community ~visibility:"public" ~indexable:true ~network:true
    ~onboarding:"published" ~discoverable:true conn slug

let legacy conn slug =
  insert_community ~visibility:"public" ~indexable:true ~network:false
    ~onboarding:"published" ~discoverable:true conn slug

let add_top_mod conn ~user ~community =
  exec conn "top_mod fixture" Community_fixture.q_insert_moderator
    (user, community, "top_mod")

(* === cases === *)

let detail_draft_case =
  db_case "legacy guard: a forged /update-community against a network setup \
           draft writes nothing and answers the generic community 404"
    (fun ~url conn ->
      let* actor = insert_user conn "ncpg_detail" in
      let* community = draft conn "ncpg-detail-draft" in
      let* () = add_top_mod conn ~user:actor ~community in
      let* before = state conn community in
      (* A durable moderator, a session admin, and both at once: the guard
         precedes every authorization branch, so none of them writes. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, admin_session) ->
            let* response, body =
              post ~url ~target:"/update-community" ~multipart:true ~user:actor
                ~admin_session
                (detail_fields ~community_id:community
                   ~community_slug:"ncpg-detail-draft"
                   ~description:"Forged draft description.")
            in
            Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
            check_opaque label body;
            unchanged label conn community before)
          [ ("top mod", false); ("session admin", true) ]
      in
      (* Browser-supplied fields cannot steer the guard: the write target
         is the id, and a mismatched slug does not move it. *)
      let* legacy_community = legacy conn "ncpg-detail-legacy" in
      let* response, body =
        post ~url ~target:"/update-community" ~multipart:true ~user:actor
          ~admin_session:true
          (detail_fields ~community_id:community
             ~community_slug:"ncpg-detail-legacy"
             ~description:"Forged through a legacy slug.")
      in
      Alcotest.(check int) "mismatched slug: 404" 404 (status_of response);
      check_opaque "mismatched slug" body;
      let* () = unchanged "mismatched slug" conn community before in
      (* And the legacy community named in the forged field is untouched
         too — the guard refused before any write at all. *)
      let* legacy_state = state conn legacy_community in
      Alcotest.(check bool) "legacy community untouched" true
        (Html_assert.contains legacy_state "<null>");
      Lwt.return_unit)

let detail_published_case =
  db_case "legacy guard: /update-community on a published network community \
           enforces the canonical identity policy instead of relying on the \
           database CHECK" (fun ~url conn ->
      let* actor = insert_user conn "ncpg_pub" in
      let* community = published_network conn "ncpg-pub" in
      let* () = add_top_mod conn ~user:actor ~community in
      let* before = state conn community in
      (* A description the scoped CHECK would reject fails closed as a
         generic form error, with no write and no constraint name. *)
      let* () =
        Lwt_list.iter_s
          (fun (label, description) ->
            let* response, body =
              post ~url ~target:"/update-community" ~multipart:true ~user:actor
                (detail_fields ~community_id:community ~community_slug:"ncpg-pub"
                   ~description)
            in
            Alcotest.(check int) (label ^ ": 400") 400 (status_of response);
            Alcotest.(check bool)
              (label ^ ": generic copy")
              true
              (Html_assert.contains body "There was a problem with your submission.");
            check_opaque label body;
            unchanged label conn community before)
          [ ("control byte", "body\x01here");
            ("invalid utf-8", "body\xffhere");
            ("over the scalar limit", Home_provisioning_fixture.phvf_repeat Home_provisioning_fixture.phvf_scalar 2001)
          ]
      in
      (* A canonical description is accepted and stored canonically —
         permitted editing is not broken, only made canonical. *)
      let* response, _body =
        post ~url ~target:"/update-community" ~multipart:true ~user:actor
          (detail_fields ~community_id:community ~community_slug:"ncpg-pub"
             ~description:"  Published body.\r\nSecond line.  ")
      in
      Alcotest.(check bool) "canonical write redirects" true
        (status_of response >= 300 && status_of response < 400);
      let* after = state conn community in
      Alcotest.(check bool) "LF-normalized and trimmed" true
        (Html_assert.contains after "|Published body.\nSecond line.|");
      Lwt.return_unit)

let detail_legacy_case =
  db_case "legacy guard: /update-community on a legacy community behaves \
           exactly as before, including values the network policy would \
           reject" (fun ~url conn ->
      let* actor = insert_user conn "ncpg_legacy" in
      let* community = legacy conn "ncpg-legacy-detail" in
      let* () = add_top_mod conn ~user:actor ~community in
      (* A control-bearing description is legacy-legal (no CHECK scopes it)
         and must still be written by the untouched legacy path. *)
      let* response, _body =
        post ~url ~target:"/update-community" ~multipart:true ~user:actor
          (detail_fields ~community_id:community
             ~community_slug:"ncpg-legacy-detail"
             ~description:"legacy\x01body")
      in
      Alcotest.(check bool) "legacy write redirects" true
        (status_of response >= 300 && status_of response < 400);
      let* after = state conn community in
      Alcotest.(check bool) "legacy value stored verbatim" true
        (Html_assert.contains after "legacy\x01body");
      (* A nonexistent community id keeps the old silent no-op redirect —
         for an actor who really carries global authority. No per-community
         moderator row can exist for an id that does not, so the durable
         users.is_admin row (not the session claim) is what admits this
         request at all. *)
      let* () = exec conn "grant admin" Community_fixture.q_set_admin (actor, true) in
      let* response, _body =
        post ~url ~target:"/update-community" ~multipart:true ~user:actor
          ~admin_session:true
          (detail_fields ~community_id:2147483000 ~community_slug:"ncpg-gone"
             ~description:"nothing")
      in
      Alcotest.(check bool) "missing id still redirects" true
        (status_of response >= 300 && status_of response < 400);
      Lwt.return_unit)

let visibility_case =
  db_case "legacy guard: the visibility route refuses a network setup draft \
           and never transitions it" (fun ~url conn ->
      let* actor = insert_user conn "ncpg_vis" in
      let* community = draft conn "ncpg-vis-draft" in
      let* () = add_top_mod conn ~user:actor ~community in
      let* before = state conn community in
      let* () =
        Lwt_list.iter_s
          (fun (label, value, admin_session) ->
            let* response, body =
              post ~url ~target:"/c/ncpg-vis-draft/settings/visibility"
                ~user:actor ~admin_session
                [ ("visibility", value) ]
            in
            Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
            check_opaque label body;
            unchanged label conn community before)
          [ ("top mod to public", "public", false);
            ("session admin to public", "public", true);
            ("top mod to private", "private", false)
          ]
      in
      (* The refusal is byte-identical to a missing community, so the route
         cannot be used to probe lifecycle. *)
      let* _response, missing_body =
        post ~url ~target:"/c/ncpg-nothing/settings/visibility" ~user:actor
          [ ("visibility", "public") ]
      in
      let* _response, draft_body =
        post ~url ~target:"/c/ncpg-vis-draft/settings/visibility" ~user:actor
          [ ("visibility", "public") ]
      in
      (* Framework CSRF fields carry random bytes, so they are dropped
         before the byte-identity comparison. *)
      Alcotest.(check string) "draft answers like a missing community"
        (Html_assert.without_csrf_inputs missing_body)
        (Html_assert.without_csrf_inputs draft_body);
      (* A legacy community still transitions exactly as before. *)
      let* legacy_community = legacy conn "ncpg-vis-legacy" in
      let* () = add_top_mod conn ~user:actor ~community:legacy_community in
      let* response, _body =
        post ~url ~target:"/c/ncpg-vis-legacy/settings/visibility" ~user:actor
          [ ("visibility", "private") ]
      in
      Alcotest.(check bool) "legacy transition redirects" true
        (status_of response >= 300 && status_of response < 400);
      let* after = state conn legacy_community in
      Alcotest.(check bool) "legacy is now private" true
        (Html_assert.contains after "|private|");
      Lwt.return_unit)

let indexability_case =
  db_case "legacy guard: the discovery route refuses every network \
           community, draft or published, and never flips the flag"
    (fun ~url conn ->
      let* actor = insert_user conn "ncpg_idx" in
      let* draft_community = draft conn "ncpg-idx-draft" in
      let* published = published_network conn "ncpg-idx-pub" in
      let* () = add_top_mod conn ~user:actor ~community:draft_community in
      let* () = add_top_mod conn ~user:actor ~community:published in
      let* draft_before = state conn draft_community in
      let* published_before = state conn published in
      let* () =
        Lwt_list.iter_s
          (fun (label, slug, id, before, value) ->
            let* response, body =
              post ~url
                ~target:(Printf.sprintf "/c/%s/settings/indexability" slug)
                ~user:actor ~admin_session:true
                [ ("indexable", value) ]
            in
            Alcotest.(check int) (label ^ ": 404") 404 (status_of response);
            check_opaque label body;
            unchanged label conn id before)
          [ ("draft to indexable", "ncpg-idx-draft", draft_community,
             draft_before, "true");
            ("draft to non-indexable", "ncpg-idx-draft", draft_community,
             draft_before, "false");
            ("published to non-indexable", "ncpg-idx-pub", published,
             published_before, "false");
            ("published to indexable", "ncpg-idx-pub", published,
             published_before, "true")
          ]
      in
      (* A legacy community still toggles exactly as before. *)
      let* legacy_community = legacy conn "ncpg-idx-legacy" in
      let* () = add_top_mod conn ~user:actor ~community:legacy_community in
      let* response, _body =
        post ~url ~target:"/c/ncpg-idx-legacy/settings/indexability"
          ~user:actor [ ("indexable", "false") ]
      in
      Alcotest.(check bool) "legacy toggle redirects" true
        (status_of response >= 300 && status_of response < 400);
      let* after = state conn legacy_community in
      Alcotest.(check bool) "legacy is now non-indexable" true
        (Html_assert.contains after "|public|published|false|false|true");
      Lwt.return_unit)

let no_publication_route_case =
  db_case "legacy guard: no route publishes a network draft — the setup \
           form's action is deliberately unregistered" (fun ~url conn ->
      let* actor = insert_user conn "ncpg_pubroute" in
      let* community = draft conn "ncpg-pubroute" in
      let* () = add_top_mod conn ~user:actor ~community in
      let* before = state conn community in
      (* Every guarded route, plus the unregistered publication target
         itself: after all of them the draft is byte-identically a draft. *)
      let* _ =
        post ~url ~target:"/c/ncpg-pubroute/settings/visibility" ~user:actor
          ~admin_session:true [ ("visibility", "public") ]
      in
      let* _ =
        post ~url ~target:"/c/ncpg-pubroute/settings/indexability" ~user:actor
          ~admin_session:true [ ("indexable", "true") ]
      in
      let* response, _body =
        post ~url ~target:"/c/ncpg-pubroute/publish" ~user:actor
          ~admin_session:true
          [ ("community_name", "Ncpg Published");
            ("community_slug", "ncpg-pubroute");
            ("community_description", "");
            ("publication_visibility", "public")
          ]
      in
      Alcotest.(check int) "publication route unregistered" 404
        (status_of response);
      unchanged "after every attempt" conn community before)

let suite =
  [ detail_draft_case; detail_published_case; detail_legacy_case;
    visibility_case; indexability_case; no_publication_route_case
  ]

let suites =
    (* Legacy mutation bypass closure: the three existing identity and
       lifecycle routes refuse a network community before any write rather
       than becoming a scoped CHECK violation, forged browser fields cannot
       steer them, the generic responses disclose no lifecycle or
       authorization detail, and legacy communities behave exactly as
       before. Database-gated. *)
  [ ("network_community_legacy_mutation_guards", suite)
  ]
