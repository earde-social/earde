module AnT = Earde.Analytics.For_testing

(* Real handlers over a real DB (EARDE_TEST_DATABASE_URL gate): each case runs
   the actual Dream handler behind sql_pool + memory_sessions with a valid
   CSRF token injected into the body, asserting the events the success path
   emits through the sink and the silence of every failure path. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM thread_source_messages WHERE post_id IN (SELECT id FROM posts WHERE title LIKE 'step6 %')"
    ; "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'step6_%')"
    ; "DELETE FROM comments WHERE content LIKE 'step6 %'"
    ; "DELETE FROM posts WHERE title LIKE 'step6 %'"
    ; "DELETE FROM chat_messages WHERE content LIKE 'step6 %'"
    ; "DELETE FROM community_user_stats WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'step6_%')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
    ; "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
    ; "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || c.id::text FROM communities c WHERE c.slug LIKE 'step6-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'step6-%'"
    ; "DELETE FROM posthog_group_cleanup_jobs WHERE NOT EXISTS (SELECT 1 FROM communities c WHERE 'community:' || c.id::text = posthog_group_cleanup_jobs.group_key)"
    ; "DELETE FROM pending_signups WHERE username LIKE 'step6_%'"
    ; "DELETE FROM users WHERE username LIKE 'step6_%'"
    ]

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let or_fail_s label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label e

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
               (fun () -> f ~url conn (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_community =
  (Caqti_type.(t3 string bool string) ->! Caqti_type.int)
  "INSERT INTO communities (slug, name, sections_enabled, visibility)
   VALUES ($1, $1, $2, $3) RETURNING id"

let q_insert_post =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, community_id, user_id)
   VALUES ('step6 post', 'step6 post body', $1, $2) RETURNING id"

let q_insert_pending =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
  "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
   VALUES ($1, $1 || '@test.invalid', 'x', $2, NOW() + INTERVAL '1 hour')"

let q_comment_content =
  (Caqti_type.int ->? Caqti_type.string)
  "SELECT content FROM comments WHERE id = $1"

let q_community_id_by_slug =
  (Caqti_type.string ->? Caqti_type.int)
  "SELECT id FROM communities WHERE slug = $1"

let q_visibility_by_id =
  (Caqti_type.int ->? Caqti_type.string)
  "SELECT visibility FROM communities WHERE id = $1"

(* POST runner: presets session fields, injects a valid dream.csrf into the
   (urlencoded or multipart) body, and returns (status, sink payloads). *)
let run_handler ~url ?(consent = Some "granted") ?(session = [])
    ?(multipart = false) ?accept ~target ~form handler =
  let payloads = ref [] in
  AnT.use_enabled_test_configuration ();
  AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
  Lwt.finalize
    (fun () ->
      let pipeline =
        Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
        let* () =
          Lwt_list.iter_s
            (fun (k, v) -> Dream.set_session_field req k v)
            session
        in
        let csrf = Dream.csrf_token req in
        let fields = ("dream.csrf", csrf) :: form in
        Dream.set_body req
          (if multipart then Http_fixture.multipart_body fields else Http_fixture.encoded_form_body fields);
        handler req
      in
      let headers =
        [ ( "Content-Type",
            if multipart then
              "multipart/form-data; boundary=" ^ Http_fixture.multipart_boundary
            else "application/x-www-form-urlencoded" ) ]
        @ (match accept with Some a -> [ ("Accept", a) ] | None -> [])
        @ Analytics_fixture.consent_header consent
      in
      let request = Dream.request ~method_:`POST ~target ~headers "" in
      let* response = pipeline request in
      Lwt.return
        (Dream.status_to_int (Dream.status response), List.rev !payloads))
    (fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ();
      Lwt.return_unit)

(* GET runner (confirm-email): anonymous — no CSRF, no preset session fields
   (the session middleware itself is part of the real app pipeline). *)
let run_get_handler ~url ?(consent = Some "granted") ~target handler =
  let payloads = ref [] in
  AnT.use_enabled_test_configuration ();
  AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
  Lwt.finalize
    (fun () ->
      let pipeline = Dream.sql_pool url @@ Dream.memory_sessions @@ handler in
      let request =
        Dream.request ~method_:`GET ~target ~headers:(Analytics_fixture.consent_header consent)
          ""
      in
      let* response = pipeline request in
      Lwt.return
        (Dream.status_to_int (Dream.status response), List.rev !payloads))
    (fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ();
      Lwt.return_unit)

let check_set_keys name expected payload =
  match List.assoc_opt "$set" (Analytics_fixture.payload_props payload) with
  | Some (`Assoc set) ->
      Alcotest.(check (slist string compare))
        name expected (List.map fst set)
  | _ -> Alcotest.failf "%s: payload has no $set" name

let signup_case =
  db_case "account_signed_up once with closed $set; invalid/unconsented silent"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let hash tok = Earde.Db.pending_signup_hash_token tok in
      let* r = C.exec q_insert_pending ("step6_signup", hash "step6_tok_1") in
      let* () = or_fail "pending" r in
      let* status, payloads =
        run_get_handler ~url ~target:"/confirm?token=step6_tok_1"
          Earde.Handlers.confirm_email_handler
      in
      Alcotest.(check int) "confirm status" 200 status;
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "account_signed_up" (Analytics_fixture.event_of p);
           Alcotest.(check (slist string compare))
             "props keys" [ "user_id"; "$set"; "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           check_set_keys "closed $set"
             [ "username"; "signup_date"; "is_admin" ] p;
           (match List.assoc_opt "$set" (Analytics_fixture.payload_props p) with
            | Some (`Assoc set) ->
                Alcotest.(check (option string)) "$set username"
                  (Some "step6_signup")
                  (match List.assoc_opt "username" set with
                   | Some (`String u) -> Some u
                   | _ -> None)
            | _ -> Alcotest.fail "no $set");
           (match List.assoc_opt "user_id" (Analytics_fixture.payload_props p) with
            | Some (`Int uid) ->
                Alcotest.(check string) "distinct id"
                  ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of p)
            | _ -> Alcotest.fail "no user_id prop")
       | l -> Alcotest.failf "expected 1 signup event, got %d" (List.length l));
      (* Replay: token consumed -> `Invalid -> no event. *)
      let* status, payloads =
        run_get_handler ~url ~target:"/confirm?token=step6_tok_1"
          Earde.Handlers.confirm_email_handler
      in
      Alcotest.(check int) "replay status" 200 status;
      Alcotest.(check int) "replay emits none" 0 (List.length payloads);
      (* Fresh pending confirmed WITHOUT consent: user still created, no
         event. *)
      let* r = C.exec q_insert_pending ("step6_signup2", hash "step6_tok_2") in
      let* () = or_fail "pending 2" r in
      let* status, payloads =
        run_get_handler ~url ~consent:None ~target:"/confirm?token=step6_tok_2"
          Earde.Handlers.confirm_email_handler
      in
      Alcotest.(check int) "unconsented confirm status" 200 status;
      Alcotest.(check int) "unconsented emits none" 0 (List.length payloads);
      let* created = C.find_opt Analytics_fixture.q_username_by_id 0 in
      let* _ = or_fail "noop lookup" created in
      Lwt.return_unit)

let login_case =
  db_case "account_logged_in once with closed $set; bad password silent"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* hash = Earde.Auth.hash_password "step6 password" in
      let* hash = or_fail_s "hash" hash in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_login", hash) in
      let* uid = or_fail "user" uid in
      let* status, payloads =
        run_handler ~url ~target:"/login"
          ~form:
            [ ("identifier", "step6_login"); ("password", "step6 password") ]
          Earde.Handlers.login_handler
      in
      Alcotest.(check bool) "login redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "account_logged_in" (Analytics_fixture.event_of p);
           Alcotest.(check string) "distinct id"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of p);
           Alcotest.(check (slist string compare))
             "person fields only inside $set"
             [ "user_id"; "$set"; "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           check_set_keys "closed $set"
             [ "username"; "signup_date"; "is_admin" ] p
       | l -> Alcotest.failf "expected 1 login event, got %d" (List.length l));
      let* status, payloads =
        run_handler ~url ~target:"/login"
          ~form:[ ("identifier", "step6_login"); ("password", "wrong") ]
          Earde.Handlers.login_handler
      in
      Alcotest.(check int) "failed login status" 200 status;
      Alcotest.(check int) "failed login emits none" 0 (List.length payloads);
      Lwt.return_unit)

let join_case =
  db_case
    "join emits community_joined + $groupidentify; private/denied silent"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      ignore conn;
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_joiner", "x") in
      let* uid = or_fail "user" uid in
      let* pub = C.find q_insert_community ("step6-pub", true, "public") in
      let* pub = or_fail "public community" pub in
      let* priv = C.find q_insert_community ("step6-priv", true, "private") in
      let* priv = or_fail "private community" priv in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_joiner") ]
      in
      let* status, payloads =
        run_handler ~url ~session ~target:"/join"
          ~form:
            [ ("community_id", string_of_int pub); ("redirect_to", "/feed") ]
          Earde.Handlers.join_community_handler
      in
      Alcotest.(check bool) "join redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ joined; gi ] ->
           Alcotest.(check string) "event" "community_joined"
             (Analytics_fixture.event_of joined);
           Alcotest.(check string) "joined distinct"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of joined);
           Alcotest.(check (option string)) "$groups key"
             (Some ("community:" ^ string_of_int pub))
             (Analytics_fixture.an_group_key joined);
           Alcotest.(check (slist string compare))
             "joined props keys"
             [ "user_id"; "community_id"; "community_slug";
               "community_visibility"; "$groups"; "deployment_environment" ]
             (Analytics_fixture.prop_keys joined);
           Alcotest.(check string) "groupidentify event" "$groupidentify"
             (Analytics_fixture.event_of gi);
           Alcotest.(check string) "groupidentify distinct is the USER"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of gi);
           Alcotest.(check (option string)) "group key"
             (Some ("community:" ^ string_of_int pub))
             (Analytics_fixture.group_key_prop_of gi);
           Alcotest.(check (slist string compare))
             "closed group props (no created_at on the record)"
             [ "community_id"; "community_slug"; "community_name";
               "community_visibility" ]
             (List.map fst (Analytics_fixture.group_set_of gi))
       | l ->
           Alcotest.failf "expected joined+groupidentify, got %d payloads"
             (List.length l));
      (* Private community: same 404 as missing; nothing emitted. *)
      let* status, payloads =
        run_handler ~url ~session ~target:"/join"
          ~form:
            [ ("community_id", string_of_int priv); ("redirect_to", "/feed") ]
          Earde.Handlers.join_community_handler
      in
      Alcotest.(check int) "private join 404" 404 status;
      Alcotest.(check int) "private join emits none" 0 (List.length payloads);
      (* Denied consent: the join itself still succeeds, zero events. *)
      let* status, payloads =
        run_handler ~url ~session ~consent:(Some "denied") ~target:"/join"
          ~form:
            [ ("community_id", string_of_int pub); ("redirect_to", "/feed") ]
          Earde.Handlers.join_community_handler
      in
      Alcotest.(check bool) "denied join still redirects" true
        (Http_fixture.is_redirect status);
      Alcotest.(check int) "denied join emits none" 0 (List.length payloads);
      Lwt.return_unit)

let leave_case =
  db_case "leave emits community_left only when a row was really deleted"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_leaver", "x") in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_insert_community ("step6-leave", true, "public") in
      let* cid = or_fail "community" cid in
      let* r = Earde.Db.join_community conn uid cid in
      let* () = or_fail_s "join fixture" r in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_leaver") ]
      in
      let leave () =
        run_handler ~url ~session ~target:"/leave"
          ~form:
            [ ("community_id", string_of_int cid); ("redirect_to", "/feed") ]
          Earde.Handlers.leave_community_handler
      in
      let* status, payloads = leave () in
      Alcotest.(check bool) "leave redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "community_left" (Analytics_fixture.event_of p);
           Alcotest.(check (slist string compare))
             "props keys"
             [ "user_id"; "community_id"; "$groups";
               "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           Alcotest.(check (option string)) "$groups key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.an_group_key p)
       | l -> Alcotest.failf "expected 1 leave event, got %d" (List.length l));
      (* Leaving again as a non-member: identical product response (the
         DELETE matches zero rows), but no community_left event. *)
      let* status, payloads = leave () in
      Alcotest.(check bool) "no-op leave still redirects" true
        (Http_fixture.is_redirect status);
      Alcotest.(check int) "no-op leave emits none" 0 (List.length payloads);
      Lwt.return_unit)

let visibility_case =
  db_case "visibility change emits one $groupidentify with the new value"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_visadmin", "x") in
      let* uid = or_fail "user" uid in
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      let* cid = C.find q_insert_community ("step6-vis", true, "public") in
      let* cid = or_fail "community" cid in
      let router =
        Dream.router
          [ Dream.post "/c/:slug/settings/visibility"
              Earde.Handlers.update_community_visibility_handler
          ]
      in
      let submit ?consent ~session value =
        run_handler ~url ?consent ~session
          ~target:"/c/step6-vis/settings/visibility"
          ~form:[ ("visibility", value) ]
          router
      in
      let admin_session =
        [ ("user_id", string_of_int uid); ("username", "step6_visadmin");
          ("is_admin", "true") ]
      in
      let* status, payloads = submit ~session:admin_session "private" in
      Alcotest.(check bool) "visibility change redirects" true
        (Http_fixture.is_redirect status);
      (match payloads with
       | [ gi ] ->
           Alcotest.(check string) "event" "$groupidentify" (Analytics_fixture.event_of gi);
           Alcotest.(check string) "distinct is the acting user, not synthetic"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of gi);
           Alcotest.(check (option string)) "group key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.group_key_prop_of gi);
           Alcotest.(check (option string)) "NEW visibility in $group_set"
             (Some "private")
             (match List.assoc_opt "community_visibility" (Analytics_fixture.group_set_of gi) with
              | Some (`String v) -> Some v
              | _ -> None);
           (* The switch is TO private, so the §13 redaction applies to
              this very $groupidentify: no readable identifiers, no
              indexability — only the numeric id and the new closed
              visibility value. *)
           Alcotest.(check (slist string compare))
             "closed group props only (private: no slug/name/indexability)"
             [ "community_id"; "community_visibility" ]
             (List.map fst (Analytics_fixture.group_set_of gi))
       | l ->
           Alcotest.failf "expected 1 groupidentify, got %d" (List.length l));
      (* Forbidden: neither admin nor top mod -> 403, silent, value kept. *)
      let* nobody = C.find Analytics_fixture.q_insert_user ("step6_visnobody", "x") in
      let* nobody = or_fail "nobody" nobody in
      let* status, payloads =
        submit
          ~session:
            [ ("user_id", string_of_int nobody);
              ("username", "step6_visnobody") ]
          "public"
      in
      Alcotest.(check int) "forbidden status" 403 status;
      Alcotest.(check int) "forbidden emits none" 0 (List.length payloads);
      (* Invalid value: validation error, silent. *)
      let* status, payloads = submit ~session:admin_session "friends-only" in
      Alcotest.(check int) "invalid value status" 400 status;
      Alcotest.(check int) "invalid value emits none" 0 (List.length payloads);
      (* Denied consent: the mutation still succeeds, zero events. *)
      let* status, payloads =
        submit ~consent:(Some "denied") ~session:admin_session "public"
      in
      Alcotest.(check bool) "denied consent still redirects" true
        (Http_fixture.is_redirect status);
      Alcotest.(check int) "denied consent emits none" 0 (List.length payloads);
      let* stored = C.find_opt q_visibility_by_id cid in
      let* stored = or_fail "stored visibility" stored in
      Alcotest.(check (option string))
        "denied-consent mutation really applied" (Some "public") stored;
      Lwt.return_unit)

let chat_case =
  db_case "chat_message_sent: json and redirect modes each emit exactly once"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_chatter", "x") in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_insert_community ("step6-chat", true, "public") in
      let* cid = or_fail "community" cid in
      let* r = Earde.Db.join_community conn uid cid in
      let* () = or_fail_s "membership" r in
      let* chslug = Earde.Db.create_channel conn cid "general" None 0 in
      let* chslug = or_fail_s "channel" chslug in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_chatter") ]
      in
      let send ?accept content =
        run_handler ~url ~session ?accept ~target:"/send-message"
          ~form:
            [ ("community_slug", "step6-chat"); ("channel_slug", chslug);
              ("content", content) ]
          Earde.Handlers.send_message_handler
      in
      let* status, payloads = send "step6 hello redirect" in
      Alcotest.(check bool) "redirect mode redirects" true
        (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "chat_message_sent" (Analytics_fixture.event_of p);
           Alcotest.(check (option string)) "response_mode"
             (Some "redirect")
             (match List.assoc_opt "response_mode" (Analytics_fixture.payload_props p) with
              | Some (`String m) -> Some m
              | _ -> None);
           Alcotest.(check (option string)) "$groups key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.an_group_key p);
           Alcotest.(check (slist string compare))
             "props keys"
             [ "user_id"; "community_id"; "community_slug"; "channel_id";
               "channel_slug"; "message_id"; "content_length";
               "response_mode"; "$groups"; "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           (* Length only — never the message text. *)
           Alcotest.(check (option int)) "content_length"
             (Some (String.length "step6 hello redirect"))
             (match List.assoc_opt "content_length" (Analytics_fixture.payload_props p) with
              | Some (`Int n) -> Some n
              | _ -> None)
       | l -> Alcotest.failf "redirect mode: expected 1, got %d" (List.length l));
      let* status, payloads =
        send ~accept:"application/json" "step6 hello json"
      in
      Alcotest.(check int) "json mode 200" 200 status;
      (match payloads with
       | [ p ] ->
           Alcotest.(check (option string)) "response_mode json"
             (Some "json")
             (match List.assoc_opt "response_mode" (Analytics_fixture.payload_props p) with
              | Some (`String m) -> Some m
              | _ -> None)
       | l -> Alcotest.failf "json mode: expected 1, got %d" (List.length l));
      Lwt.return_unit)

let post_case =
  db_case "forum_thread_created once on success; non-member silent"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_poster", "x") in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_insert_community ("step6-post", false, "public") in
      let* cid = or_fail "community" cid in
      let* r = Earde.Db.join_community conn uid cid in
      let* () = or_fail_s "membership" r in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_poster") ]
      in
      let form =
        [ ("title", "step6 post title"); ("content", "step6 body");
          ("community_id", string_of_int cid) ]
      in
      let* status, payloads =
        run_handler ~url ~session ~multipart:true ~target:"/create-post"
          ~form Earde.Handlers.create_post_handler
      in
      Alcotest.(check bool) "post redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "forum_thread_created" (Analytics_fixture.event_of p);
           Alcotest.(check (slist string compare))
             "props keys (no title/body/url)"
             [ "user_id"; "community_id"; "post_id"; "content_length";
               "has_link"; "has_mention"; "$groups";
               "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           Alcotest.(check (option string)) "$groups key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.an_group_key p)
       | l -> Alcotest.failf "expected 1 post event, got %d" (List.length l));
      (* Non-member: refused, silent. *)
      let* other = C.find Analytics_fixture.q_insert_user ("step6_stranger", "x") in
      let* other = or_fail "other" other in
      let* status, payloads =
        run_handler ~url
          ~session:
            [ ("user_id", string_of_int other);
              ("username", "step6_stranger") ]
          ~multipart:true ~target:"/create-post" ~form
          Earde.Handlers.create_post_handler
      in
      Alcotest.(check int) "non-member status" 200 status;
      Alcotest.(check int) "non-member emits none" 0 (List.length payloads);
      Lwt.return_unit)

let comment_case =
  db_case "forum_comment_created carries the real RETURNING comment id"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_commenter", "x") in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_insert_community ("step6-comm", false, "public") in
      let* cid = or_fail "community" cid in
      (* Commenting now requires a current participation path (origin
         membership here) — the Shared Threads server-side rule. *)
      let* r = Earde.Db.join_community conn uid cid in
      let* () = or_fail_s "membership" r in
      let* pid = C.find q_insert_post (cid, uid) in
      let* pid = or_fail "post" pid in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_commenter") ]
      in
      let* status, payloads =
        run_handler ~url ~session ~target:"/create-comment"
          ~form:
            [ ("content", "step6 comment"); ("post_id", string_of_int pid) ]
          Earde.Handlers.create_comment_handler
      in
      Alcotest.(check bool) "comment redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "forum_comment_created" (Analytics_fixture.event_of p);
           Alcotest.(check (slist string compare))
             "props keys (top-level comment: no parent_comment_id)"
             [ "user_id"; "community_id"; "post_id"; "comment_id";
               "content_length"; "has_mention"; "$groups";
               "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           (match List.assoc_opt "comment_id" (Analytics_fixture.payload_props p) with
            | Some (`Int comment_id) ->
                let* row = C.find_opt q_comment_content comment_id in
                let* row = or_fail "comment row" row in
                Alcotest.(check (option string))
                  "payload comment_id is the real inserted row"
                  (Some "step6 comment") row;
                Lwt.return_unit
            | _ -> Alcotest.fail "payload has no int comment_id")
       | l ->
           Alcotest.failf "expected 1 comment event, got %d" (List.length l))
      )

let promote_case =
  db_case "conversation_promoted once with counts and group key"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_promoter", "x") in
      let* uid = or_fail "user" uid in
      let* cid = C.find q_insert_community ("step6-thr", false, "public") in
      let* cid = or_fail "community" cid in
      let* r = Earde.Db.join_community conn uid cid in
      let* () = or_fail_s "membership" r in
      let* chslug = Earde.Db.create_channel conn cid "general" None 0 in
      let* chslug = or_fail_s "channel" chslug in
      let* channel = Earde.Db.get_channel_by_slug conn chslug cid in
      let* channel = or_fail_s "channel row" channel in
      let channel =
        match channel with
        | Some ch -> ch
        | None -> Alcotest.fail "channel vanished"
      in
      let* seed = Earde.Db.send_message conn channel.Earde.Db.id uid "step6 seed" in
      let* seed = or_fail_s "seed message" seed in
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_promoter") ]
      in
      let router =
        Dream.router
          [ Dream.post
              "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
              Earde.Handlers.start_thread_create_handler
          ]
      in
      let* status, payloads =
        run_handler ~url ~session
          ~target:
            (Printf.sprintf "/c/step6-thr/ch/%s/messages/%Ld/start-thread"
               chslug seed.Earde.Db.id)
          ~form:[ ("title", "step6 thread"); ("content", "") ]
          router
      in
      Alcotest.(check bool) "promotion redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ p ] ->
           Alcotest.(check string) "event" "conversation_promoted" (Analytics_fixture.event_of p);
           Alcotest.(check string) "distinct"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of p);
           Alcotest.(check (slist string compare))
             "props keys (sectionless community)"
             [ "user_id"; "community_id"; "community_slug"; "channel_id";
               "channel_slug"; "post_id"; "message_id";
               "promoted_message_count"; "promoted_participant_count";
               "$groups"; "deployment_environment" ]
             (Analytics_fixture.prop_keys p);
           Alcotest.(check (option string)) "$groups key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.an_group_key p);
           Alcotest.(check (option int)) "seed-only message count" (Some 1)
             (match
                List.assoc_opt "promoted_message_count" (Analytics_fixture.payload_props p)
              with
              | Some (`Int n) -> Some n
              | _ -> None);
           Alcotest.(check (option int)) "participant count" (Some 1)
             (match
                List.assoc_opt "promoted_participant_count" (Analytics_fixture.payload_props p)
              with
              | Some (`Int n) -> Some n
              | _ -> None)
       | l ->
           Alcotest.failf "expected 1 promotion event, got %d" (List.length l));
      Lwt.return_unit)

let create_community_case =
  db_case "community creation emits exactly one $groupidentify (no event)"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_founder", "x") in
      let* uid = or_fail "user" uid in
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      (* Legacy community creation is admin-gated on the DURABLE
         users.is_admin row, which the session claim only enables the lookup
         for; the founder must really be an admin to reach the analytics
         behavior under test. *)
      let session =
        [ ("user_id", string_of_int uid); ("username", "step6_founder");
          ("is_admin", "true") ]
      in
      let* status, payloads =
        run_handler ~url ~session ~target:"/create-community"
          ~form:
            [ ("name", "step6-created"); ("slug", "step6-created");
              ("section_count", "0") ]
          Earde.Handlers.create_community_handler
      in
      Alcotest.(check bool) "creation redirects" true (Http_fixture.is_redirect status);
      let* cid = C.find_opt q_community_id_by_slug "step6-created" in
      let* cid = or_fail "created community" cid in
      let cid =
        match cid with Some id -> id | None -> Alcotest.fail "no community"
      in
      (match payloads with
       | [ gi ] ->
           Alcotest.(check string) "only $groupidentify" "$groupidentify"
             (Analytics_fixture.event_of gi);
           Alcotest.(check string) "distinct is the creator"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of gi);
           Alcotest.(check (option string)) "group key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.group_key_prop_of gi)
       | l ->
           Alcotest.failf "expected exactly 1 groupidentify, got %d: %s"
             (List.length l)
             (String.concat ", " (List.map Analytics_fixture.event_of l)));
      Lwt.return_unit)

let update_settings_case =
  db_case "settings update emits $groupidentify; forbidden silent"
    (fun ~url _conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_moddy", "x") in
      let* uid = or_fail "user" uid in
      let* r = C.exec Analytics_fixture.q_make_admin uid in
      let* () = or_fail "durable admin" r in
      let* cid = C.find q_insert_community ("step6-upd", true, "public") in
      let* cid = or_fail "community" cid in
      let form =
        [ ("community_id", string_of_int cid);
          ("community_slug", "step6-upd");
          ("description", "step6 new description"); ("rules", "");
          ("avatar_url", ""); ("banner_url", "");
          ("existing_avatar_url", ""); ("existing_banner_url", "") ]
      in
      let admin_session =
        [ ("user_id", string_of_int uid); ("username", "step6_moddy");
          ("is_admin", "true") ]
      in
      let* status, payloads =
        run_handler ~url ~session:admin_session ~multipart:true
          ~target:"/update-community" ~form
          Earde.Handlers.update_community_handler
      in
      Alcotest.(check bool) "update redirects" true (Http_fixture.is_redirect status);
      (match payloads with
       | [ gi ] ->
           Alcotest.(check string) "event" "$groupidentify" (Analytics_fixture.event_of gi);
           Alcotest.(check string) "distinct is the acting admin"
             ("user:" ^ string_of_int uid) (Analytics_fixture.distinct_of gi);
           Alcotest.(check (option string)) "group key"
             (Some ("community:" ^ string_of_int cid))
             (Analytics_fixture.group_key_prop_of gi);
           Alcotest.(check (slist string compare))
             "closed group props"
             [ "community_id"; "community_slug"; "community_name";
               "community_visibility" ]
             (List.map fst (Analytics_fixture.group_set_of gi))
       | l ->
           Alcotest.failf "expected 1 groupidentify, got %d" (List.length l));
      (* Unauthorized (neither admin nor moderator): 403, silent. *)
      let* other = C.find Analytics_fixture.q_insert_user ("step6_nobody", "x") in
      let* other = or_fail "other" other in
      let* status, payloads =
        run_handler ~url
          ~session:
            [ ("user_id", string_of_int other); ("username", "step6_nobody") ]
          ~multipart:true ~target:"/update-community" ~form
          Earde.Handlers.update_community_handler
      in
      Alcotest.(check int) "forbidden status" 403 status;
      Alcotest.(check int) "forbidden emits none" 0 (List.length payloads);
      Lwt.return_unit)

let delete_account_case =
  db_case "account_deleted once with the pre-anonymization id"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* uid = C.find Analytics_fixture.q_insert_user ("step6_deleteme", "x") in
      let* uid = or_fail "user" uid in
      let did = "user:" ^ string_of_int uid in
      (* Step 7 moved the capture into the post-response async cleanup chain
         (capture → claim → deletion attempt), so this runner keeps the sink
         and configuration installed until the chain has finished — tracked
         by the durable job row acquiring its safe last_error (no deletion
         credentials are configured here, so the attempt must leave the job
         pending with missing_configuration). *)
      let payloads = ref [] in
      AnT.use_enabled_test_configuration ();
      AnT.set_capture_sink (fun p -> payloads := !payloads @ [ p ]);
      Lwt.finalize
        (fun () ->
          let pipeline =
            Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
            let* () =
              Dream.set_session_field req "user_id" (string_of_int uid)
            in
            let* () =
              Dream.set_session_field req "username" "step6_deleteme"
            in
            let csrf = Dream.csrf_token req in
            Dream.set_body req (Http_fixture.encoded_form_body [ ("dream.csrf", csrf) ]);
            Earde.Handlers.delete_account_handler req
          in
          let request =
            Dream.request ~method_:`POST ~target:"/delete-account"
              ~headers:
                ([ ("Content-Type", "application/x-www-form-urlencoded") ]
                @ Analytics_fixture.consent_header (Some "granted"))
              ""
          in
          let* response = pipeline request in
          Alcotest.(check bool) "deletion redirects" true
            (Http_fixture.is_redirect (Dream.status_to_int (Dream.status response)));
          let* () =
            Analytics_fixture.wait_until ~label:"post-deletion cleanup chain" (fun () ->
                let* job = Earde.Db.get_posthog_deletion_job conn did in
                match job with
                | Ok (Some (_, _, _, Some _)) -> Lwt.return true
                | _ -> Lwt.return false)
          in
          (match !payloads with
           | [ p ] ->
               Alcotest.(check string) "event" "account_deleted" (Analytics_fixture.event_of p);
               Alcotest.(check string) "constant non-user distinct id"
                 Earde.Analytics.account_deletion_distinct_id (Analytics_fixture.distinct_of p);
               Alcotest.(check (slist string compare))
                 "personless: person processing off, plus the envelope"
                 [ "$process_person_profile"; "deployment_environment" ]
                 (Analytics_fixture.prop_keys p);
               Alcotest.(check bool) "no user identity in the metric" false
                 (Html_assert.contains (Yojson.Safe.to_string p) did)
           | l ->
               Alcotest.failf "expected 1 deletion metric, got %d"
                 (List.length l));
          let* job = Earde.Db.get_posthog_deletion_job conn did in
          let* job = or_fail_s "job row" job in
          let* job_id =
            match job with
            | Some (job_id, status, attempts, last_error) ->
                Alcotest.(check string) "job stays durably pending" "pending"
                  status;
                Alcotest.(check int) "one immediate attempt" 1 attempts;
                Alcotest.(check (option string)) "safe config marker"
                  (Some "missing_configuration") last_error;
                Lwt.return job_id
            | None -> Alcotest.fail "no durable deletion job"
          in
          let* name = C.find_opt Analytics_fixture.q_username_by_id uid in
          let* name = or_fail "anonymized row" name in
          Alcotest.(check (option string)) "row anonymized"
            (Some (Printf.sprintf "[deleted_%d]" uid))
            name;
          (* The anonymized username no longer matches the step6_ cleanup
             pattern — drop the job and the row here. *)
          let* r = C.exec Analytics_fixture.q_delete_job job_id in
          let* () = or_fail "drop job" r in
          let* r = C.exec Analytics_fixture.q_delete_user uid in
          let* () = or_fail "drop anonymized user" r in
          Lwt.return_unit)
        (fun () ->
          AnT.clear_capture_sink ();
          AnT.clear_configuration_override ();
          Lwt.return_unit))

let q_set_avatar =
  (Caqti_type.(t2 (option string) int) ->. Caqti_type.unit)
  "UPDATE users SET avatar_url = $1, bio = 'step6 bio' WHERE id = $2"

let q_bio_avatar_by_id =
  (Caqti_type.int ->? Caqti_type.(t2 (option string) (option string)))
  "SELECT bio, avatar_url FROM users WHERE id = $1"

(* Account deletion's file cleanup: the validated local upload disappears,
   a bystander's file survives, a missing file and an external URL are both
   harmless, and bio/avatar_url are scrubbed in the same transaction.
   Fixture files live under the test CWD's static/uploads — the same
   relative root production resolves — inside dune's _build sandbox, never
   the source tree. *)
let delete_account_avatar_case =
  db_case "account deletion removes only its own validated avatar file"
    (fun ~url conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let (_ : int) = Sys.command "mkdir -p static/uploads" in
      let write path =
        let oc = open_out_bin path in
        output_string oc "step6 webp bytes";
        close_out oc
      in
      let a_file = "static/uploads/earde_991000_000001.webp" in
      let b_file = "static/uploads/earde_991000_000002.webp" in
      write a_file;
      write b_file;
      let* a = C.find Analytics_fixture.q_insert_user ("step6_avatar_a", "x") in
      let* a = or_fail "user a" a in
      let* b = C.find Analytics_fixture.q_insert_user ("step6_avatar_b", "x") in
      let* b = or_fail "user b" b in
      let* c_missing = C.find Analytics_fixture.q_insert_user ("step6_avatar_c", "x") in
      let* c_missing = or_fail "user c" c_missing in
      let* d_external = C.find Analytics_fixture.q_insert_user ("step6_avatar_d", "x") in
      let* d_external = or_fail "user d" d_external in
      let set uid value =
        let* r = C.exec q_set_avatar (value, uid) in
        or_fail "avatar fixture" r
      in
      let* () = set a (Some "/static/uploads/earde_991000_000001.webp") in
      let* () = set b (Some "/static/uploads/earde_991000_000002.webp") in
      let* () =
        set c_missing (Some "/static/uploads/earde_991000_000404.webp")
      in
      let* () =
        set d_external (Some "https://cdn.example.com/earde_1_2.webp")
      in
      let run_delete uid name =
        let pipeline =
          Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
          let* () =
            Dream.set_session_field req "user_id" (string_of_int uid)
          in
          let* () = Dream.set_session_field req "username" name in
          let csrf = Dream.csrf_token req in
          Dream.set_body req (Http_fixture.encoded_form_body [ ("dream.csrf", csrf) ]);
          Earde.Handlers.delete_account_handler req
        in
        pipeline
          (Dream.request ~method_:`POST ~target:"/delete-account"
             ~headers:
               [ ("Content-Type", "application/x-www-form-urlencoded") ]
             "")
      in
      let check_scrubbed label uid =
        let* profile = C.find_opt q_bio_avatar_by_id uid in
        let* profile = or_fail (label ^ " row") profile in
        match profile with
        | Some (bio, avatar) ->
            Alcotest.(check (option string)) (label ^ " bio cleared") None
              bio;
            Alcotest.(check (option string)) (label ^ " avatar cleared")
              None avatar;
            Lwt.return_unit
        | None -> Alcotest.failf "%s row missing" label
      in
      let drop_job_and_user uid =
        let did = "user:" ^ string_of_int uid in
        let* job = Earde.Db.get_posthog_deletion_job conn did in
        let* job = or_fail_s "job row" job in
        let* () =
          match job with
          | Some (job_id, _, _, _) ->
              let* r = C.exec Analytics_fixture.q_delete_job job_id in
              or_fail "drop job" r
          | None -> Lwt.return_unit
        in
        let* r = C.exec Analytics_fixture.q_delete_user uid in
        or_fail "drop user" r
      in
      Lwt.finalize
        (fun () ->
          (* A: a real local upload — its file must go, B's must stay. *)
          let* response = run_delete a "step6_avatar_a" in
          Alcotest.(check bool) "A: deletion redirects" true
            (Http_fixture.is_redirect (Dream.status_to_int (Dream.status response)));
          let* () =
            Analytics_fixture.wait_until ~label:"A avatar file removed" (fun () ->
                Lwt.return (not (Sys.file_exists a_file)))
          in
          Alcotest.(check bool) "bystander file untouched" true
            (Sys.file_exists b_file);
          let* () = check_scrubbed "A" a in
          (* C: the stored URL's file never existed — deletion still
             succeeds. *)
          let* response = run_delete c_missing "step6_avatar_c" in
          Alcotest.(check bool) "C: deletion redirects" true
            (Http_fixture.is_redirect (Dream.status_to_int (Dream.status response)));
          let* () = check_scrubbed "C" c_missing in
          (* D: an external URL never reaches the filesystem. *)
          let* response = run_delete d_external "step6_avatar_d" in
          Alcotest.(check bool) "D: deletion redirects" true
            (Http_fixture.is_redirect (Dream.status_to_int (Dream.status response)));
          let* () = check_scrubbed "D" d_external in
          Alcotest.(check bool) "bystander file still present at the end"
            true
            (Sys.file_exists b_file);
          Lwt.return_unit)
        (fun () ->
          (try Sys.remove a_file with _ -> ());
          (try Sys.remove b_file with _ -> ());
          (* Anonymized rows no longer match the step6_ cleanup pattern —
             drop their jobs and rows here; B keeps its step6_ name and is
             swept by the module cleanup. *)
          let* () = drop_job_and_user a in
          let* () = drop_job_and_user c_missing in
          drop_job_and_user d_external))

let suite =
  [ signup_case; login_case; join_case; leave_case; chat_case; post_case
  ; comment_case; promote_case; create_community_case; update_settings_case
  ; visibility_case; delete_account_case; delete_account_avatar_case
  ]

let suites =
  [ ( "analytics_step6_events", suite )
  ]
