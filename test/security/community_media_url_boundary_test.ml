(* Community media URL boundary.

   The community settings form used to round-trip the stored avatar and banner
   URLs back to the browser through hidden inputs, and /update-community used
   whatever came back as its no-upload fallback. Any moderator could therefore
   store an arbitrary external URL as a community's avatar or banner. The
   public community page renders both through safe_img_src, which permits any
   https origin, so every visitor of that community — anonymous ones included —
   would fetch it, handing a third party their IP, User-Agent and Referer.

   The fallback now comes from the community row the handler already loaded.
   Both fields are covered here on purpose: a fix that closed only the avatar
   path would leave the same beacon reachable through the banner. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let must = Security_fixture.must

let must_not = Security_fixture.must_not

let contains = Html_assert.contains

let community_slug = "cmub-media"

let mod_username = "cmub_mod"

(* The two values a client would try to smuggle in. Both are shapes the
   renderer would happily emit if they ever reached the database: an
   external https origin, and a well-formed uploads path this community
   does not own. Neither may survive the handler. *)
let forged_avatar = "https://attacker.invalid/beacon.png"

let forged_banner = "/static/uploads/earde_1700000009999_999001.webp"

(* Authoritative stored values. Deliberately NOT pipeline-shaped: they name
   no local file, so nothing on disk corresponds to them for any code path
   to create, keep or delete. They still render — safe_img_src admits any
   https origin — so the two "not vacuous" render assertions below keep
   their full force, and the reserved .invalid TLD cannot resolve, so
   nothing ever fetches them. *)
let stored_avatar = "https://stored.invalid/cmub-avatar.png"

let stored_banner = "https://stored.invalid/cmub-banner.png"

(* The peer every /update-community request below is sent from. The handler
   hands Dream.client verbatim to process_image_upload, so this exact
   string — port included — is the ip_address the upload limiter writes and
   the one the cleanup deletes. Pinning it is what makes the bucket owned by
   this suite rather than shared with every other suite that happens to use
   Dream's default test peer. 198.51.100.0/24 is reserved for documentation
   and is never contacted. *)
let client_peer = "198.51.100.242:4242"

(* === Case 1: the form itself, DB-free === *)

let cmub_secret = "cmub-test-secret-value"

let form_community : Earde.Db.community =
  { id = 7710; slug = community_slug; name = "Cmub Media";
    description = None; rules = None;
    avatar_url = Some stored_avatar; banner_url = Some stored_banner;
    allow_downvotes = true; sections_enabled = false;
    visibility = Earde.Db.Community_public; indexable = true;
    is_network_community = false;
    onboarding_state = Earde.Db.Community_published; discoverable = true }

let render_settings () =
  let captured = ref None in
  let pipeline =
    Dream.set_secret cmub_secret @@ Dream.memory_sessions
    @@ fun req ->
    captured :=
      Some
        (Earde.Pages.community_settings_page ~is_admin:false ~is_top_mod:true
           ~open_reports_count:0 ~community:form_community ~mods:[]
           ~banned_users:[] ~members:[] ~sections:[] ~channels:[] req);
    Dream.html ""
  in
  ignore
    (Lwt_main.run
       (pipeline
          (Dream.request ~method_:`GET
             ~target:("/c/" ^ community_slug ^ "/settings?panel=profile") "")));
  match !captured with
  | Some html -> html
  | None -> Alcotest.fail "settings renderer did not run"

let form_fields_case =
  Alcotest.test_case
    "settings form: no avatar or banner fallback field is rendered, and \
     both previews and file inputs remain" `Quick (fun () ->
      let html = render_settings () in
      (* The record carries BOTH stored values, so their absence from any
         hidden input is the fix rather than an empty fixture. *)
      must "avatar preview" html stored_avatar;
      must "banner preview" html stored_banner;
      must "avatar preview note" html "Current avatar uploaded";
      must "banner preview note" html "Current banner uploaded";
      must "avatar file input" html
        "<input type='file' name='avatar_url' accept='image/*' class='cm-file'>";
      must "banner file input" html
        "<input type='file' name='banner_url' accept='image/*' class='cm-file'>";
      must_not "avatar fallback field" html "existing_avatar_url";
      must_not "banner fallback field" html "existing_banner_url")

(* === gated fixtures === *)

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let q_user =
  (Caqti_type.string ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $1 || '@cmub.invalid', 'x', TRUE) RETURNING id"

let q_community =
  (Caqti_type.(t3 string (option string) (option string)) ->! Caqti_type.int)
  "INSERT INTO communities (slug, name, visibility, avatar_url, banner_url)
   VALUES ($1, 'Cmub Media', 'public', $2, $3) RETURNING id"

let q_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_moderators (user_id, community_id) VALUES ($1, $2)
   ON CONFLICT DO NOTHING"

let q_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
  "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)
   ON CONFLICT DO NOTHING"

let q_media =
  (Caqti_type.int ->! Caqti_type.(t2 (option string) (option string)))
  "SELECT avatar_url, banner_url FROM communities WHERE id = $1"

(* Exact ip and exact purpose: the upload limiter's bucket for this suite's
   own fixed peer, never another suite's and never the whole table. *)
let q_clear_upload_rate =
  (Caqti_type.string ->. Caqti_type.unit)
  "DELETE FROM rate_limits WHERE ip_address = $1 AND endpoint = 'image-upload'"

(* Exact slug and exact username throughout; the one LIKE is over an
   explicitly escaped namespace, matching the session payload this suite
   writes and nothing else. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM community_user_stats WHERE community_id IN (SELECT id FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || id::text FROM communities WHERE slug = 'cmub-media')"
    ; "DELETE FROM communities WHERE slug = 'cmub-media'"
    ; "DELETE FROM dream_session WHERE payload LIKE '%cmub\\_mod%'"
    ; "DELETE FROM users WHERE username = 'cmub_mod'"
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
               let* () =
                 Lwt_list.iter_s
                   (fun q ->
                     let* r = C.exec q () in
                     let* _ = or_fail "cleanup" r in
                     Lwt.return_unit)
                   q_cleanup
               in
               (* Runs before the case and again in the finalizer, so the
                  row an upload writes is gone after a pass and after an
                  assertion failure alike. Deleting when no row exists is a
                  harmless no-op. *)
               let* r = C.exec q_clear_upload_rate client_peer in
               or_fail "clear upload rate" r
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* === the routed pipeline === *)

let next_uid = ref (None : int option)

let shared_pipeline = ref None

let pipeline_for ~url =
  match !shared_pipeline with
  | Some p -> p
  | None ->
      let p =
        Dream.sql_pool ~size:4 url @@ Dream.set_secret cmub_secret
        @@ Dream.sql_sessions
        @@ Dream.router
             [ (* Mints the durable session a real login would create, and
                  hands back a CSRF token minted inside it. *)
               Dream.get "/session" (fun req ->
                   let* () =
                     match !next_uid with
                     | None -> Lwt.return_unit
                     | Some uid ->
                         let* () =
                           Dream.set_session_field req "user_id"
                             (string_of_int uid)
                         in
                         Dream.set_session_field req "username" mod_username
                   in
                   Dream.respond (Dream.csrf_token req));
               Dream.get "/token" (fun req ->
                   Dream.respond (Dream.csrf_token req));
               Dream.post "/update-community"
                 Earde.Handlers.update_community_handler;
               Dream.get "/c/:slug" Earde.Handlers.community_page_handler
             ]
      in
      shared_pipeline := Some p;
      p

let session_cookie label response =
  match
    List.find_opt
      (fun v -> contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | None -> Alcotest.fail (label ^ ": no session cookie")
  | Some v -> (
      match String.index_opt v ';' with
      | Some i -> String.sub v 0 i
      | None -> v)

let login ~url uid =
  next_uid := Some uid;
  let p = pipeline_for ~url in
  let* response = p (Dream.request ~method_:`GET ~target:"/session" "") in
  let cookie = session_cookie "session" response in
  let* token = Dream.body response in
  next_uid := None;
  Lwt.return (cookie, token)

let boundary = "cmubboundary"

let part_field (k, v) =
  Printf.sprintf
    "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
    boundary k v

let part_file (field, filename, bytes) =
  Printf.sprintf
    "--%s\r\nContent-Disposition: form-data; name=\"%s\"; \
     filename=\"%s\"\r\nContent-Type: image/png\r\n\r\n%s\r\n"
    boundary field filename bytes

let multipart_body ~files fields =
  String.concat "" (List.map part_field fields)
  ^ String.concat "" (List.map part_file files)
  ^ Printf.sprintf "--%s--\r\n" boundary

let post_update ~url ~cookie ~token ?(files = []) fields =
  let p = pipeline_for ~url in
  let body = multipart_body ~files (("dream.csrf", token) :: fields) in
  let headers =
    [ ("Content-Type", "multipart/form-data; boundary=" ^ boundary);
      ("Cookie", cookie)
    ]
  in
  let request =
    Dream.request ~method_:`POST ~target:"/update-community" ~headers ""
  in
  (* Pin the peer before the pipeline sees it: the handler passes
     Dream.client straight to the upload limiter, so this is what makes the
     bucket test-owned and exactly cleanable. *)
  Dream.set_client request client_peer;
  Dream.set_body request body;
  let* response = p request in
  let* rbody = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), response, rbody)

(* Anonymous, exactly like the visitor the beacon would have targeted. *)
let get_public_page ~url =
  let p = pipeline_for ~url in
  let* response =
    p (Dream.request ~method_:`GET ~target:("/c/" ^ community_slug) "")
  in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

(* One authorized moderator on one community that already owns both stored
   values — the starting state every gated case needs, except that one
   case stores its avatar as SQL NULL to prove an absent value stays
   absent. *)
let fixture ?(avatar = Some stored_avatar) (module C : Caqti_lwt.CONNECTION)
    =
  let* uid = C.find q_user mod_username in
  let* uid = or_fail "user" uid in
  let* cid =
    C.find q_community (community_slug, avatar, Some stored_banner)
  in
  let* cid = or_fail "community" cid in
  let* r = C.exec q_member (uid, cid) in
  let* () = or_fail "member" r in
  let* r = C.exec q_moderator (uid, cid) in
  let* () = or_fail "moderator" r in
  Lwt.return (uid, cid)

let read_media (module C : Caqti_lwt.CONNECTION) cid =
  let* r = C.find q_media cid in
  or_fail "media" r

(* === Case 2: forged fallback fields are ignored === *)

let forged_fallback_case =
  db_case
    "update-community: submitted avatar and banner fallback URLs are \
     ignored and never reach the database or the public page"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid, cid = fixture (module C) in
      let* cookie, token = login ~url uid in
      let* status, response, _ =
        post_update ~url ~cookie ~token
          [ ("community_id", string_of_int cid);
            ("community_slug", community_slug);
            ("description", "cmub description");
            ("rules", "");
            (* No file part for either field: this is the no-upload path. *)
            ("avatar_url", "");
            ("banner_url", "");
            ("existing_avatar_url", forged_avatar);
            ("existing_banner_url", forged_banner)
          ]
      in
      (* The success semantics are unchanged: still a 303 to the
         authoritative settings URL. *)
      Alcotest.(check int) "303" 303 status;
      Alcotest.(check (option string))
        "authoritative Location"
        (Some ("/c/" ^ community_slug ^ "/settings"))
        (Dream.header response "Location");
      let* avatar, banner = read_media (module C) cid in
      Alcotest.(check (option string))
        "stored avatar unchanged" (Some stored_avatar) avatar;
      Alcotest.(check (option string))
        "stored banner unchanged" (Some stored_banner) banner;
      Alcotest.(check bool)
        "forged avatar is not stored" false
        (avatar = Some forged_avatar);
      Alcotest.(check bool)
        "forged banner is not stored" false
        (banner = Some forged_banner);
      let* page_status, page = get_public_page ~url in
      Alcotest.(check int) "public page renders" 200 page_status;
      (* Non-vacuous: the page really does emit both stored values, so the
         two absences below are the boundary and not an empty page. *)
      must "public page shows the stored avatar" page stored_avatar;
      must "public page shows the stored banner" page stored_banner;
      must_not "forged avatar reaches no visitor" page forged_avatar;
      must_not "forged banner reaches no visitor" page forged_banner;
      Lwt.return_unit)

(* === Case 3: real uploads still replace both values === *)

(* The handler writes static/uploads relative to the process CWD, and under
   `dune exec` that CWD is wherever the command was typed — the repository
   root included. Neither source of names a shared directory offers can
   establish ownership: the row read after a failed request (an avatar
   converted before the banner was refused, or both converted before the
   write failed) still names the old values, not the files just written,
   and a before/after listing could claim a file another process wrote in
   the meantime. So the case runs with its CWD inside a directory created
   under a fresh unique name. Nothing else knows that path, so every file
   that appears there — on success or on any partial failure — was written
   by this request, and nothing outside it can become an unlink target. *)
let with_private_uploads f =
  let root = Filename.temp_dir "earde-cmub-" "" in
  let static = Filename.concat root "static" in
  let uploads = Filename.concat static "uploads" in
  let home = Sys.getcwd () in
  (* Only pipeline-shaped regular files are removed, by exact name, and the
     directories go with a plain rmdir: anything unexpected makes rmdir
     fail loudly and stays in place for inspection, never swept away. *)
  let cleanup () =
    if Sys.file_exists uploads then begin
      Array.iter
        (fun name ->
          match
            Earde.Avatar_uploads.local_file_of_url ("/static/uploads/" ^ name)
          with
          | Some _ -> Sys.remove (Filename.concat uploads name)
          | None -> ())
        (Sys.readdir uploads);
      Unix.rmdir uploads
    end;
    if Sys.file_exists static then Unix.rmdir static;
    Unix.rmdir root
  in
  let* outcome =
    Lwt.catch
      (fun () ->
        Unix.mkdir static 0o700;
        Unix.mkdir uploads 0o700;
        Sys.chdir root;
        let* v = f () in
        Lwt.return (Ok v))
      (fun e -> Lwt.return (Error e))
  in
  Sys.chdir home;
  (* The case's own failure outranks a cleanup failure, which is still
     reported rather than swallowed. *)
  match (outcome, try Ok (cleanup ()) with e -> Error e) with
  | Ok v, Ok () -> Lwt.return v
  | Ok _, Error e -> Lwt.fail e
  | Error e, Ok () -> Lwt.fail e
  | Error e, Error ce ->
      prerr_endline ("cmub: private uploads cleanup: " ^ Printexc.to_string ce);
      Lwt.fail e

let local_path_of label url_value =
  match Earde.Avatar_uploads.local_file_of_url url_value with
  | Some p -> p
  | None ->
      Alcotest.failf "%s: %S is not a pipeline-shaped upload" label url_value

let new_upload_case =
  db_case
    "update-community: a real upload replaces both the avatar and the \
     banner with fresh server-generated paths"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid, cid = fixture (module C) in
      let* cookie, token = login ~url uid in
      with_private_uploads (fun () ->
          let* status, response, _ =
            post_update ~url ~cookie ~token
              ~files:
                [ ("avatar_url", "cmub-avatar.png", Security_fixture.real_png);
                  ("banner_url", "cmub-banner.png", Security_fixture.real_png)
                ]
              [ ("community_id", string_of_int cid);
                ("community_slug", community_slug);
                ("description", "cmub description");
                ("rules", "");
                (* Present and hostile, to prove the new values come from
                   the pipeline and not from anything submitted. *)
                ("existing_avatar_url", forged_avatar);
                ("existing_banner_url", forged_banner)
              ]
          in
          Alcotest.(check int) "303" 303 status;
          Alcotest.(check (option string))
            "authoritative Location"
            (Some ("/c/" ^ community_slug ^ "/settings"))
            (Dream.header response "Location");
          let* avatar, banner = read_media (module C) cid in
          let avatar =
            match avatar with
            | Some a -> a
            | None -> Alcotest.fail "avatar was cleared instead of replaced"
          in
          let banner =
            match banner with
            | Some b -> b
            | None -> Alcotest.fail "banner was cleared instead of replaced"
          in
          (* Fresh, rooted, server-generated: neither the previous stored
             value nor either submitted fallback decided the result. *)
          must "avatar is a rooted uploads path" avatar
            "/static/uploads/earde_";
          must "banner is a rooted uploads path" banner
            "/static/uploads/earde_";
          Alcotest.(check bool) "avatar replaced" false
            (String.equal avatar stored_avatar);
          Alcotest.(check bool) "banner replaced" false
            (String.equal banner stored_banner);
          Alcotest.(check bool) "avatar is not the submitted fallback" false
            (String.equal avatar forged_avatar);
          Alcotest.(check bool) "banner is not the submitted fallback" false
            (String.equal banner forged_banner);
          (* The pipeline mints one destination name per conversion. *)
          Alcotest.(check bool) "avatar and banner are distinct files" false
            (String.equal avatar banner);
          let avatar_path = local_path_of "avatar" avatar in
          let banner_path = local_path_of "banner" banner in
          Alcotest.(check bool) "avatar file exists" true
            (Sys.file_exists avatar_path);
          Alcotest.(check bool) "banner file exists" true
            (Sys.file_exists banner_path);
          (* And those two are all it wrote: the row names every file the
             request produced, so no conversion was orphaned. *)
          Alcotest.(check (list string))
            "the request wrote exactly the two files the row names"
            (List.sort String.compare
               [ Filename.basename avatar_path; Filename.basename banner_path ])
            (List.sort String.compare
               (Array.to_list (Sys.readdir (Filename.dirname avatar_path))));
          Lwt.return_unit))

(* === Cases 4 and 5: one upload replaces only its own field === *)

(* Exactly one file part. The other file input still arrives, empty, as a
   browser sends an unselected one, and both legacy fallback fields carry
   hostile values, so the untouched field can only come from the row. *)
let post_single_upload ~url ~cookie ~token ~cid ~uploaded ~empty =
  post_update ~url ~cookie ~token
    ~files:[ (uploaded, "cmub-single.png", Security_fixture.real_png) ]
    [ ("community_id", string_of_int cid);
      ("community_slug", community_slug);
      ("description", "cmub description");
      ("rules", "");
      (empty, "");
      ("existing_avatar_url", forged_avatar);
      ("existing_banner_url", forged_banner)
    ]

let check_authoritative_success status response =
  Alcotest.(check int) "303" 303 status;
  Alcotest.(check (option string))
    "authoritative Location"
    (Some ("/c/" ^ community_slug ^ "/settings"))
    (Dream.header response "Location")

(* A fresh pipeline value: accepted by the real upload-path parser, backed
   by a file this request wrote, and neither submitted fallback — the
   forged banner is itself pipeline-shaped, so the parser alone would not
   exclude it. The private uploads directory must then hold that one file
   and nothing else: a single conversion, no orphan. *)
let check_sole_fresh_upload label value =
  let v =
    match value with
    | Some v -> v
    | None -> Alcotest.failf "%s was cleared instead of replaced" label
  in
  Alcotest.(check bool) (label ^ " is not the submitted avatar fallback")
    false (String.equal v forged_avatar);
  Alcotest.(check bool) (label ^ " is not the submitted banner fallback")
    false (String.equal v forged_banner);
  let path = local_path_of label v in
  Alcotest.(check bool) (label ^ " file exists") true (Sys.file_exists path);
  Alcotest.(check (list string))
    ("the request wrote exactly the one file the " ^ label ^ " names")
    [ Filename.basename path ]
    (List.sort String.compare
       (Array.to_list (Sys.readdir (Filename.dirname path))))

let avatar_only_case =
  db_case
    "update-community: an avatar-only upload replaces the avatar and keeps \
     the stored banner"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid, cid = fixture (module C) in
      let* cookie, token = login ~url uid in
      with_private_uploads (fun () ->
          let* status, response, _ =
            post_single_upload ~url ~cookie ~token ~cid
              ~uploaded:"avatar_url" ~empty:"banner_url"
          in
          check_authoritative_success status response;
          let* avatar, banner = read_media (module C) cid in
          check_sole_fresh_upload "avatar" avatar;
          Alcotest.(check (option string))
            "banner kept from the stored row" (Some stored_banner) banner;
          Lwt.return_unit))

let banner_only_null_avatar_case =
  db_case
    "update-community: a banner-only upload replaces the banner and keeps \
     a NULL avatar NULL"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid, cid = fixture ~avatar:None (module C) in
      (* The precondition is the row itself, not an empty rendering. *)
      let* before = read_media (module C) cid in
      Alcotest.(check (pair (option string) (option string)))
        "precondition: avatar is SQL NULL, banner is stored"
        (None, Some stored_banner) before;
      let* cookie, token = login ~url uid in
      with_private_uploads (fun () ->
          let* status, response, _ =
            post_single_upload ~url ~cookie ~token ~cid
              ~uploaded:"banner_url" ~empty:"avatar_url"
          in
          check_authoritative_success status response;
          let* avatar, banner = read_media (module C) cid in
          (* Exactly None: an empty string, or either forged value, is a
             different option and fails here. *)
          Alcotest.(check (option string))
            "avatar stays SQL NULL" None avatar;
          check_sole_fresh_upload "banner" banner;
          Lwt.return_unit))

let suite =
  [ form_fields_case; forged_fallback_case; new_upload_case;
    avatar_only_case; banner_only_null_avatar_case ]

let suites =
    (* Community media URL boundary: with no new upload, the stored avatar
       and banner come only from the community row the handler loaded. A
       submitted fallback URL — the hidden inputs the form no longer emits —
       must reach neither the database nor any visitor's browser. *)
  [ ("security_community_media_url_boundary", suite)
  ]
