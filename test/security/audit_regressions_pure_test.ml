(* === PRE-LAUNCH SECURITY FIXES: the DB-free half ===

   One regression per confirmed audit finding, reproducing the original
   attack rather than merely asserting the fixed behaviour. The pure
   decisions (escaping, username syntax, upload policy, cookie policy,
   client-address resolution) live here; the database-backed halves are in
   [Sec_db] below, behind the usual EARDE_TEST_DATABASE_URL gate. *)

let case name f = Alcotest.test_case name `Quick f
let req target = Dream.request ~method_:`GET ~target ""

(* Pages that emit a CSRF field need a request that has been through
   session middleware; Dream.csrf_tag raises otherwise. Same shape as the
   existing launch-message wrapper suite. *)
let render_with_session target f =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun r ->
           rendered := f r;
           Dream.html "")
         (Dream.request ~method_:`GET ~target ""))
  in
  !rendered

(* --- Fix 2: reflected XSS through msg_page's return_url ---------------- *)

(* The original exploit: GET /c/<payload>/t/z answered 404 with the payload
   interpolated raw into the "Go back" href. The payload arrives through
   Dream.param, which percent-decodes, so quotes and angle brackets reach
   the renderer intact. *)
let msg_page_hostile_return_url =
  case "msg_page: a quote+markup return_url emits no raw tag" (fun () ->
      let payload = "/c/x'><svg onload=alert(document.domain)>/t/z" in
      let body =
        Earde.Site_pages.msg_page ~title:"Not Found"
          ~message:"This community does not exist." ~alert_type:"error"
          ~return_url:payload (req "/")
      in
      (* The document's own alert glyph is an <svg>, so the assertion is on
         the PAYLOAD: no attribute-closing quote, no event handler, no
         second element smuggled in through the href. *)
      Security_fixture.must_not "hostile return_url" body "<svg onload";
      Security_fixture.must_not "hostile return_url" body "x'>";
      (* The whole payload survives ONLY as the escaped text of one href
         attribute value it cannot close, which is the precise property
         that makes it inert. *)
      Security_fixture.must "hostile return_url" body
        "href='/c/x&#39;&gt;&lt;svg onload=alert(document.domain)&gt;/t/z'";
      Security_fixture.must "hostile return_url" body "launch-msg__back")

let msg_page_rejects_foreign_targets =
  case "msg_page: javascript:, protocol-relative and foreign URLs collapse"
    (fun () ->
      let render return_url =
        Earde.Site_pages.msg_page ~title:"T" ~message:"M" ~alert_type:"error"
          ~return_url (req "/")
      in
      List.iter
        (fun (label, hostile) ->
          let body = render hostile in
          Security_fixture.must_not label body "href='javascript:";
          Security_fixture.must_not label body "href='//evil.example";
          Security_fixture.must_not label body "href='https://evil.example";
          Security_fixture.must_not label body "href='/\\\\evil.example";
          Security_fixture.must label body "href='#'")
        [
          ("javascript:", "javascript:alert(1)");
          ("protocol-relative", "//evil.example/x");
          ("backslash variant", "/\\evil.example/x");
          ("absolute foreign", "https://evil.example/x");
          ("empty", "");
          ("bare fragment", "#");
        ])

let msg_page_preserves_internal_paths =
  case "msg_page: ordinary internal back links are unchanged" (fun () ->
      List.iter
        (fun path ->
          let body =
            Earde.Site_pages.msg_page ~title:"T" ~message:"M"
              ~alert_type:"error" ~return_url:path (req "/")
          in
          Security_fixture.must ("internal " ^ path) body
            (Printf.sprintf "href='%s'" path))
        [
          "/";
          "/c/example";
          "/c/example/settings";
          "/settings";
          "/p/12";
          "/c/example/t/12-a-thread";
        ])

(* --- Fix 3: username escaping and the new signup syntax ---------------- *)

let js_attr_escaping =
  case "confirm hooks: user data never enters script source" (fun () ->
      let hostile =
        [ "x'); alert(1); ('"; "</script><svg onload=alert(1)>"; "a\\b\"c\nd" ]
      in
      let banned =
        List.mapi
          (fun i username ->
            ({
               id = 900 + i;
               username;
               email = Printf.sprintf "u%d@example.invalid" i;
             }
              : Earde.User_store.user))
          hostile
      in
      let page =
        let rendered = ref "" in
        let (_ : Dream.response) =
          Lwt_main.run
            (Dream.memory_sessions
               (fun req ->
                 let ( let* ) = Lwt.bind in
                 let* () = Dream.set_session_field req "user_id" "1" in
                 rendered :=
                   Earde.Admin_pages.admin_dashboard_page ~user:"qa-admin"
                     ~signups_enabled:true ~turnstile:`Configured
                     ~brevo_configured:true ~recent_users:[] ~pending:[]
                     ~banned_users:banned req;
                 Dream.html "")
               (Dream.request ~method_:`GET ~target:"/admin" ""))
        in
        !rendered
      in
      (* Every hook is the one constant script; the names live only in
         escaped attribute text. *)
      Alcotest.(check int)
        "one hook per banned user" (List.length hostile)
        (Html_assert.occurrences page
           "onsubmit=\"confirmModal(event, this.dataset.confirm)\"");
      Alcotest.(check int)
        "no call passes a script literal" 0
        (Html_assert.occurrences page "confirmModal(event, '");
      Security_fixture.must "apostrophe" page
        "data-confirm='Lift global ban on u/x&#39;); alert(1); (&#39;?'";
      Security_fixture.must_not "markup" page "<svg onload";
      Security_fixture.must_not "script end" page "</script><svg";
      Security_fixture.must "quote" page "a\\b&quot;c")

let username_syntax =
  case "is_valid_new_username: route-safe ASCII only" (fun () ->
      let ok = Earde.Auth_handlers.is_valid_new_username in
      List.iter
        (fun name -> Alcotest.(check bool) ("accepted: " ^ name) true (ok name))
        [ "alice"; "Alice"; "alice_1"; "a-b-c"; "ABC123"; "_x"; "x-" ];
      List.iter
        (fun name ->
          Alcotest.(check bool)
            ("rejected: " ^ String.escaped name)
            false (ok name))
        [
          "";
          "x'><svg onload=alert(1)>";
          "a b";
          "a\tb";
          "a\nb";
          "a/b";
          "a\\b";
          "a\"b";
          "a'b";
          "a<b";
          "a>b";
          "a&b";
          "a?b";
          "a#b";
          "a%b";
          "a.b";
          "a@b";
          "a\x00b";
          "a\x7fb";
          "ünïcode";
        ])

(* The stored-XSS half: an account whose hostile name predates the syntax
   rule must still render inert on its public, crawlable profile. *)
let hostile_username_profile_render =
  case "user_profile_page: a stored hostile username renders inert" (fun () ->
      let hostile = "x'><svg onload=alert(1)>" in
      let render ~is_admin ~viewer =
        render_with_session ("/u/" ^ hostile) (fun r ->
            Earde.Account_pages.user_profile_page ?user:viewer ~is_admin
              ~is_globally_banned:false ~profile_id:7 ~admin_usernames:[]
              ~moderated_communities:[] ~active_tab:"posts" [] hostile
              "2026-01-01 00:00:00" (Some "a bio") None 0 [] [] [] r)
      in
      (* Anonymous view: heading and the three tab links. *)
      let anon = render ~is_admin:false ~viewer:None in
      (* The launch chrome renders its own <svg> icons, so the assertion is
         on the PAYLOAD: it must never appear with live syntax, and it must
         appear escaped in the heading and in all three tab links. *)
      Security_fixture.must_not "anonymous profile" anon "<svg onload";
      Security_fixture.must_not "anonymous profile" anon "x'>";
      Security_fixture.must "anonymous profile" anon
        "u/x&#39;&gt;&lt;svg onload=alert(1)&gt;";
      Security_fixture.must "anonymous profile" anon
        "href='/u/x&#39;&gt;&lt;svg onload=alert(1)&gt;?tab=posts'";
      Security_fixture.must "anonymous profile" anon
        "href='/u/x&#39;&gt;&lt;svg onload=alert(1)&gt;?tab=comments'";
      Security_fixture.must "anonymous profile" anon
        "href='/u/x&#39;&gt;&lt;svg onload=alert(1)&gt;?tab=communities'";
      (* Admin view additionally renders the ban confirmation hook. Its
         script source is a constant; the name only travels in the
         escaped data attribute the hook reads. *)
      let admin = render ~is_admin:true ~viewer:(Some "someadmin") in
      Security_fixture.must_not "admin profile" admin "<svg onload";
      Security_fixture.must_not "admin profile" admin "x'>";
      Security_fixture.must "admin profile" admin
        "onsubmit=\"confirmModal(event, this.dataset.confirm)\"";
      Security_fixture.must "admin profile" admin
        "data-confirm='Permanently ban u/x&#39;&gt;&lt;svg onload=alert(1)&gt;?";
      Security_fixture.must_not "admin profile" admin "confirmModal(event, '")

let ordinary_username_profile_render =
  case "user_profile_page: ordinary tabs and controls still target the user"
    (fun () ->
      let body =
        render_with_session "/u/alice" (fun r ->
            Earde.Account_pages.user_profile_page ~user:"root" ~is_admin:true
              ~is_globally_banned:false ~profile_id:9 ~admin_usernames:[]
              ~moderated_communities:[] ~active_tab:"posts" [] "alice"
              "2026-01-01 00:00:00" None None 0 [] [] [] r)
      in
      Security_fixture.must "tabs" body "href='/u/alice?tab=posts'";
      Security_fixture.must "tabs" body "href='/u/alice?tab=comments'";
      Security_fixture.must "tabs" body "href='/u/alice?tab=communities'";
      Security_fixture.must "heading" body "u/alice";
      Security_fixture.must "admin control" body "action='/admin/ban/user/9'";
      Security_fixture.must "admin control" body "ban u/alice?")

(* --- Fix 1: the settings form no longer round-trips the avatar URL ----- *)

let settings_form_has_no_avatar_input =
  case "settings_page: no existing_avatar_url field is rendered" (fun () ->
      let body =
        render_with_session "/settings" (fun r ->
            Earde.Account_pages.settings_page ~user:"alice" (Some "bio")
              (Some "/static/uploads/earde_1_2.webp") r)
      in
      Security_fixture.must_not "settings form" body "existing_avatar_url";
      (* The upload control and the read-only preview both remain. *)
      Security_fixture.must "settings form" body "name='avatar_url'";
      Security_fixture.must "settings form" body
        "/static/uploads/earde_1_2.webp")

(* --- Fix 6: production-aware Secure session cookie -------------------- *)

let cookie_policy_origin_rule =
  case "session cookie: Secure follows the configured public origin" (fun () ->
      let p = Earde.Session_cookie_policy.secure_required in
      Alcotest.(check bool) "https origin" true (p (Some "https://earde.com"));
      Alcotest.(check bool)
        "https with path" true
        (p (Some "https://earde.com/"));
      Alcotest.(check bool)
        "http origin" false
        (p (Some "http://localhost:8080"));
      Alcotest.(check bool) "unset" false (p None);
      Alcotest.(check bool) "empty" false (p (Some ""));
      (* Never derived from a forwarded header, so a client-supplied
         "https" spelling in some other field cannot matter. *)
      Alcotest.(check bool) "nonsense" false (p (Some "https-ish")))

let cookie_policy_attribute =
  case "session cookie: Secure is appended exactly once" (fun () ->
      let add = Earde.Session_cookie_policy.add_secure_attribute in
      let has = Earde.Session_cookie_policy.has_secure_attribute in
      let plain =
        "dream.session=abc; Max-Age=1209599; Path=/; HttpOnly; SameSite=Lax"
      in
      let secured = add plain in
      Security_fixture.must "adds Secure" secured "; Secure";
      Security_fixture.must "keeps HttpOnly" secured "HttpOnly";
      Security_fixture.must "keeps SameSite" secured "SameSite=Lax";
      Security_fixture.must "keeps Max-Age" secured "Max-Age=1209599";
      Security_fixture.must "keeps the name" secured "dream.session=abc";
      (* Idempotent, and case-insensitive about an existing attribute. *)
      Alcotest.(check string) "idempotent" secured (add secured);
      Alcotest.(check bool) "detects lowercase" true (has "a=b; path=/; secure");
      (* A cookie whose VALUE merely contains the word is not already
         secure — the check is anchored to ';'-separated attributes. *)
      Alcotest.(check bool)
        "value is not an attribute" false
        (has "dream.session=secure; Path=/");
      Security_fixture.must "value is not an attribute"
        (add "dream.session=secure; Path=/")
        "; Secure")

(* --- Fix 7: image-upload policy --------------------------------------- *)

let png_header = "\x89PNG\r\n\x1a\n\x00\x00\x00\x0dIHDR"
let jpeg_header = "\xff\xd8\xff\xe0\x00\x10JFIF"
let gif_header = "GIF89a\x01\x00\x01\x00"
let webp_header = "RIFF\x24\x00\x00\x00WEBPVP8 "

let upload_format_gate =
  case "image upload: the format gate reads magic bytes, not labels" (fun () ->
      let d = Earde.Image_upload.detect_format in
      Alcotest.(check bool)
        "png" true
        (d png_header = Some Earde.Image_upload.Png);
      Alcotest.(check bool)
        "jpeg" true
        (d jpeg_header = Some Earde.Image_upload.Jpeg);
      Alcotest.(check bool)
        "gif87a" true
        (d "GIF87a\x01\x00" = Some Earde.Image_upload.Gif);
      Alcotest.(check bool)
        "gif89a" true
        (d gif_header = Some Earde.Image_upload.Gif);
      Alcotest.(check bool)
        "webp" true
        (d webp_header = Some Earde.Image_upload.Webp);
      List.iter
        (fun (label, payload) ->
          Alcotest.(check bool) ("rejected: " ^ label) true (d payload = None))
        [
          ("empty", "");
          ("truncated png", "\x89PN");
          ("truncated webp riff", "RIFFabc");
          ("riff that is not webp", "RIFF\x24\x00\x00\x00WAVEfmt ");
          ( "svg",
            "<?xml version=\"1.0\"?><svg xmlns='http://www.w3.org/2000/svg'/>"
          );
          ("svg no prologue", "<svg onload=alert(1)></svg>");
          ( "imagemagick MSL",
            "<?xml version=\"1.0\"?><image><read filename=\"x\"/></image>" );
          ("imagemagick MVG", "push graphic-context\nviewbox 0 0 1 1\n");
          ("pdf", "%PDF-1.7\n");
          ("postscript", "%!PS-Adobe-3.0\n");
          ("elf", "\x7fELF\x02\x01\x01");
          ("shell script", "#!/bin/sh\nrm -rf /\n");
          ("plain text", "hello");
          ("html", "<html><body>x</body></html>");
        ])

let upload_argv_is_safe =
  case "image upload: argv pins the coder, the limits and every path" (fun () ->
      let argv =
        Earde.Image_upload.convert_argv ~binary:"convert"
          ~format:Earde.Image_upload.Png ~purpose:Earde.Image_upload.Post_image
          ~input:"/tmp/earde_1_2.tmp" ~output:"/tmp/earde_1_2.webp"
      in
      let l = Array.to_list argv in
      let joined = String.concat " " l in
      Alcotest.(check string) "binary is argv0" "convert" (List.nth l 0);
      (* The input is coder-qualified, so ImageMagick never sniffs it into
         a delegate coder, and [0] keeps a multi-frame payload from fanning
         out into a directory of files. *)
      Security_fixture.must "input coder" joined "PNG:/tmp/earde_1_2.tmp[0]";
      Security_fixture.must "output coder" joined "webp:/tmp/earde_1_2.webp";
      Security_fixture.must "metadata stripped" joined "-strip";
      (* Every limit the brief requires is present. *)
      List.iter
        (fun limit -> Security_fixture.must "resource limit" joined limit)
        [ "memory"; "map"; "disk"; "area"; "width"; "height"; "time" ];
      (* Geometry is a compile-time constant per surface. *)
      Security_fixture.must "resize" joined "1920x1080>";
      Alcotest.(check string)
        "avatar geometry" "512x512>"
        (Earde.Image_upload.resize_geometry Earde.Image_upload.Profile_avatar);
      Alcotest.(check string)
        "banner geometry" "1920x480>"
        (Earde.Image_upload.resize_geometry Earde.Image_upload.Community_banner))

let upload_argv_neutralises_metacharacters =
  case "image upload: a metacharacter in a path is one inert argv element"
    (fun () ->
      (* The basename is server-minted, so this can only happen through a
         future change — but the argv form means it stays inert regardless,
         where the previous shell string depended on quoting. *)
      let nasty = "/tmp/x; rm -rf ~/`id`$(id) 'q' \"q\".tmp" in
      let argv =
        Earde.Image_upload.convert_argv ~binary:"convert"
          ~format:Earde.Image_upload.Jpeg
          ~purpose:Earde.Image_upload.Profile_avatar ~input:nasty
          ~output:"/tmp/out.webp"
      in
      let l = Array.to_list argv in
      (* Exactly one element carries the whole path; nothing is split. *)
      let carriers = List.filter (fun e -> Html_assert.contains e "rm -rf") l in
      Alcotest.(check int)
        "one argv element carries it" 1 (List.length carriers);
      Alcotest.(check string)
        "carried verbatim, coder-qualified"
        ("JPEG:" ^ nasty ^ "[0]")
        (List.hd carriers))

let upload_messages_are_uniform =
  case "image upload: refusal messages disclose nothing about the payload"
    (fun () ->
      (* One message for every refusal reason, so a payload cannot be used
         to probe what the pipeline recognises. *)
      Security_fixture.must "message" Earde.Image_upload.rejected_message
        "JPEG, PNG, GIF, WebP";
      Alcotest.(check int)
        "5 MiB cap"
        (5 * 1024 * 1024)
        Earde.Image_upload.max_bytes;
      Security_fixture.must "size message" Earde.Image_upload.too_large_message
        "5 MB")

(* --- Fix 9: the rate limiter's client identity ------------------------- *)

let peer_parsing =
  case "client address: the ephemeral source port never reaches the key"
    (fun () ->
      let p = Earde.Client_address.peer_ip in
      Alcotest.(check (option string))
        "ipv4" (Some "127.0.0.1") (p "127.0.0.1:44746");
      Alcotest.(check (option string))
        "ipv4 other port" (Some "127.0.0.1") (p "127.0.0.1:51001");
      Alcotest.(check (option string))
        "public ipv4" (Some "198.51.100.44") (p "198.51.100.44:9");
      (* Dream renders IPv6 unbracketed, so the split must be at the LAST
         colon. *)
      Alcotest.(check (option string))
        "ipv6 loopback" (Some "::1") (p "::1:44746");
      Alcotest.(check (option string))
        "ipv6 full" (Some "2001:db8::1") (p "2001:db8::1:0");
      (* A Unix-socket peer is a path, not an address. *)
      Alcotest.(check (option string)) "unix socket" None (p "/run/earde.sock");
      Alcotest.(check (option string)) "garbage" None (p "not-an-address:1"))

let normalisation_collapses_spellings =
  case "client address: alternative spellings collapse to one key" (fun () ->
      let n = Earde.Client_address.normalize_ip in
      Alcotest.(check (option string))
        "ipv6 leading zeros" (Some "::1") (n "::0001");
      Alcotest.(check (option string))
        "ipv6 canonical" (n "::1") (n "0:0:0:0:0:0:0:1");
      Alcotest.(check (option string)) "malformed" None (n "999.999.999.999");
      Alcotest.(check (option string)) "empty" None (n "");
      Alcotest.(check (option string)) "text" None (n "evil"))

let trusted = [ "127.0.0.1"; "::1" ]

let forwarded_header_is_ignored_on_direct_connections =
  case "client address: a direct client cannot nominate its own identity"
    (fun () ->
      let key ff =
        Earde.Client_address.client_ip ~trusted_proxies:trusted
          ~peer:"198.51.100.44:33001" ~forwarded_for:ff
      in
      (* The original bypass: rotate the leftmost X-Forwarded-For value and
         every request lands in a fresh bucket. From a direct peer the
         header is now ignored entirely. *)
      let keys =
        List.map
          (fun i -> key (Some (Printf.sprintf "10.0.0.%d" i)))
          [ 1; 2; 3; 4; 5; 6; 7; 8 ]
      in
      List.iter
        (fun k -> Alcotest.(check string) "one stable bucket" "198.51.100.44" k)
        keys;
      Alcotest.(check string) "no header" "198.51.100.44" (key None);
      Alcotest.(check string)
        "comma salad" "198.51.100.44"
        (key (Some "1.1.1.1, 2.2.2.2, 3.3.3.3")))

let ports_share_one_bucket =
  case "client address: one IP on many ports is one bucket" (fun () ->
      let key port =
        Earde.Client_address.client_ip ~trusted_proxies:trusted
          ~peer:(Printf.sprintf "203.0.113.7:%d" port)
          ~forwarded_for:None
      in
      List.iter
        (fun port ->
          Alcotest.(check string) "stable across ports" "203.0.113.7" (key port))
        [ 1024; 33001; 44746; 51001; 65535 ])

let trusted_proxy_uses_rightmost_entry =
  case "client address: behind a trusted proxy, only its own observation counts"
    (fun () ->
      let key ff =
        Earde.Client_address.client_ip ~trusted_proxies:trusted
          ~peer:"127.0.0.1:44746" ~forwarded_for:(Some ff)
      in
      (* nginx's proxy_add_x_forwarded_for APPENDS what it saw, so the
         rightmost entry is the trustworthy one and everything left of it
         is client-supplied noise. *)
      Alcotest.(check string)
        "single entry" "198.51.100.44" (key "198.51.100.44");
      Alcotest.(check string)
        "client-prepended noise is ignored" "198.51.100.44"
        (key "10.0.0.1, 198.51.100.44");
      Alcotest.(check string)
        "a long forged chain is ignored" "198.51.100.44"
        (key "1.1.1.1, 2.2.2.2, 3.3.3.3, 4.4.4.4, 198.51.100.44");
      Alcotest.(check string)
        "whitespace tolerated" "198.51.100.44"
        (key "  10.0.0.1 ,   198.51.100.44   ");
      (* The forged prefix cannot mint buckets: rotating it changes nothing. *)
      let rotated =
        List.map
          (fun i -> key (Printf.sprintf "10.0.0.%d, 198.51.100.44" i))
          [ 1; 2; 3; 4; 5 ]
      in
      List.iter
        (fun k -> Alcotest.(check string) "rotation is inert" "198.51.100.44" k)
        rotated)

let malformed_forwarded_fails_safe =
  case "client address: a malformed forwarded header falls back, never bypasses"
    (fun () ->
      let key ff =
        Earde.Client_address.client_ip ~trusted_proxies:trusted
          ~peer:"127.0.0.1:44746" ~forwarded_for:(Some ff)
      in
      (* Falling back to the proxy's own address gives ONE shared bucket:
         over-limiting, never a bypass. And rotating garbage cannot make
         more than that one bucket. *)
      List.iter
        (fun ff ->
          Alcotest.(check string) ("falls back: " ^ ff) "127.0.0.1" (key ff))
        [
          "";
          ",";
          "not-an-ip";
          "evil, worse";
          "999.999.999.999";
          "<script>";
          "10.0.0.1.5";
          "unknown";
        ];
      (* A malformed tail with a valid entry to its left still refuses the
         left value: only the proxy's own appended entry is trusted, and it
         is unusable here. *)
      Alcotest.(check string)
        "valid-left, garbage-right" "127.0.0.1"
        (key "198.51.100.44, garbage"))

let distinct_clients_stay_distinct =
  case "client address: different real clients keep different buckets"
    (fun () ->
      let via_proxy ip =
        Earde.Client_address.client_ip ~trusted_proxies:trusted
          ~peer:"127.0.0.1:1" ~forwarded_for:(Some ip)
      in
      let a = via_proxy "198.51.100.1" and b = via_proxy "198.51.100.2" in
      Alcotest.(check bool) "distinct" true (a <> b);
      Alcotest.(check string) "a" "198.51.100.1" a;
      Alcotest.(check string) "b" "198.51.100.2" b;
      (* IPv6 clients work the same way. *)
      Alcotest.(check string)
        "ipv6 client" "2001:db8::5" (via_proxy "2001:db8::5"))

let unparseable_peer_is_one_shared_bucket =
  case "client address: an unparseable peer shares one constant bucket"
    (fun () ->
      let key peer =
        Earde.Client_address.client_ip ~trusted_proxies:trusted ~peer
          ~forwarded_for:(Some "1.2.3.4")
      in
      Alcotest.(check string)
        "unix socket" Earde.Client_address.fallback_key (key "/run/earde.sock");
      Alcotest.(check string)
        "garbage" Earde.Client_address.fallback_key (key "???"))

let empty_trusted_set_trusts_nothing =
  case "client address: an empty trusted set ignores every forwarded header"
    (fun () ->
      Alcotest.(check string)
        "loopback peer, no trust" "127.0.0.1"
        (Earde.Client_address.client_ip ~trusted_proxies:[]
           ~peer:"127.0.0.1:44746" ~forwarded_for:(Some "198.51.100.44"));
      Alcotest.(check bool)
        "loopback is the default trusted set" true
        (Earde.Client_address.default_trusted_proxies = [ "127.0.0.1"; "::1" ]))

let escaping_suite =
  [
    msg_page_hostile_return_url;
    msg_page_rejects_foreign_targets;
    msg_page_preserves_internal_paths;
    js_attr_escaping;
    username_syntax;
    hostile_username_profile_render;
    ordinary_username_profile_render;
    settings_form_has_no_avatar_input;
  ]

let cookie_suite = [ cookie_policy_origin_rule; cookie_policy_attribute ]

let upload_suite =
  [
    upload_format_gate;
    upload_argv_is_safe;
    upload_argv_neutralises_metacharacters;
    upload_messages_are_uniform;
  ]

let client_address_suite =
  [
    peer_parsing;
    normalisation_collapses_spellings;
    forwarded_header_is_ignored_on_direct_connections;
    ports_share_one_bucket;
    trusted_proxy_uses_rightmost_entry;
    malformed_forwarded_fails_safe;
    distinct_clients_stay_distinct;
    unparseable_peer_is_one_shared_bucket;
    empty_trusted_set_trusts_nothing;
  ]

let suites =
  (* Pre-launch security fixes. The pure halves — output escaping, the
       new username syntax, the image-upload accept/argv policy, the
       session-cookie attribute rule and the client-address resolution —
       are DB-free; the reachability of each original exploit is pinned by
       the gated suites that follow. *)
  [
    ("security_escaping", escaping_suite);
    ("security_session_cookie", cookie_suite);
    ("security_image_upload_policy", upload_suite);
    ("security_client_address", client_address_suite);
  ]
