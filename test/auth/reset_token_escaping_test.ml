(* === Reset-token attribute escaping (pass 16B security fix) ===

   Auth_pages.reset_password_page re-emits the attacker-controlled reset token in a
   single-quoted hidden attribute: [<input type='hidden' name='token'
   value='...'>]. Before the fix the raw token was interpolated verbatim, so a
   token containing a quote escaped the attribute on the POST handler's
   mismatch / too-short re-render — a reflected-XSS path. These tests pin the
   escaped contract structurally: exactly one token input whose bounded value
   contains no markup-significant byte, escaping that HTML-decodes back to the
   original token losslessly, the rest of the form byte-pinned, and the same
   guarantees through the real reset_password_handler's two re-render arms
   (which never touch the database, so this suite runs ungated). *)

let ( let* ) = Lwt.bind

let case name f = Alcotest.test_case name `Quick f

(* Renders the real page through real session middleware (the page embeds a
   CSRF tag) — same harness as render_search. *)
let render_reset ?error token =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered := Earde.Auth_pages.reset_password_page ~token ?error req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/reset-password" ""))
  in
  !rendered

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

(* Drives the REAL Auth_handlers.reset_password_handler: a valid same-session
   dream.csrf is minted inside the pipeline and injected into the urlencoded
   body, exactly like the analytics POST runner. The mismatch and too-short
   arms re-render before any Dream.sql call, so no pool is mounted — if a
   future change makes these arms touch the database, this harness fails
   loudly rather than silently passing. *)
let run_reset_post ~token ~password ~confirm =
  let status = ref 0 and body_html = ref "" in
  Lwt_main.run
    (let pipeline =
       Dream.memory_sessions (fun req ->
           let csrf = Dream.csrf_token req in
           Dream.set_body req
             (form_body
                [ ("dream.csrf", csrf); ("token", token);
                  ("password", password); ("confirm_password", confirm) ]);
           Earde.Auth_handlers.reset_password_handler req)
     in
     let request =
       Dream.request ~method_:`POST ~target:"/reset-password"
         ~headers:
           [ ("Content-Type", "application/x-www-form-urlencoded") ]
         ""
     in
     let* response = pipeline request in
     status := Dream.status_to_int (Dream.status response);
     let* body = Dream.body response in
     body_html := body;
     Lwt.return_unit);
  (!status, !body_html)

(* Reference escaping (Components.html_escape's closed entity set) and its
   inverse, so tests can prove the round-trip is lossless rather than only
   spot-checking one entity. *)
let escape = Earde.Components.html_escape

let unescape s =
  let entities =
    [ ("&amp;", "&"); ("&lt;", "<"); ("&gt;", ">"); ("&quot;", "\"");
      ("&#39;", "'") ]
  in
  let buf = Buffer.create (String.length s) in
  let sl = String.length s in
  let rec go i =
    if i >= sl then Buffer.contents buf
    else
      match
        List.find_opt
          (fun (e, _) ->
            let el = String.length e in
            i + el <= sl && String.sub s i el = e)
          entities
      with
      | Some (e, raw) ->
          Buffer.add_string buf raw;
          go (i + String.length e)
      | None ->
          Buffer.add_char buf s.[i];
          go (i + 1)
  in
  go 0

let token_input_prefix = "<input type='hidden' name='token' value='"

(* The complete pinned tag for an escaped token value. *)
let token_input_of escaped = token_input_prefix ^ escaped ^ "'>"

(* Exactly one token input in the region, returned as raw tag text. *)
let token_tag region =
  match
    List.filter (fun t -> Html_assert.contains t "name='token'") (Html_assert.input_tags region)
  with
  | [ t ] -> t
  | l ->
      Alcotest.fail
        (Printf.sprintf "expected exactly one token input, found %d"
           (List.length l))

(* Structural inertness: the tag is the pinned prefix, then a bounded value
   that is the tag's LAST attribute, and the value byte-region holds no
   markup-significant character — so no smuggled attribute, no event
   handler, no autofocus, no element breakout is representable. Mirrors the
   ps_is_csrf_input technique. Returns the raw (still-escaped) value. *)
let audit_token_tag tag =
  let pl = String.length token_input_prefix and tl = String.length tag in
  Alcotest.(check bool)
    "token tag starts with the pinned prefix" true
    (tl >= pl + 2 && String.sub tag 0 pl = token_input_prefix);
  Alcotest.(check bool)
    "token tag closes immediately after the value" true
    (String.sub tag (tl - 2) 2 = "'>");
  let value = String.sub tag pl (tl - pl - 2) in
  String.iter
    (fun c ->
      if c = '\'' || c = '"' || c = '<' || c = '>' then
        Alcotest.fail
          (Printf.sprintf "markup-significant byte %C inside token value" c))
    value;
  value

(* Full audit of the reset form region for a given raw token: pinned token
   tag, lossless round-trip, one framework CSRF field, an exact input
   census (csrf + token + two pinned password fields — nothing else), the
   pinned action/method and controls, and no element injection anywhere in
   the form. *)
let audit_reset_form page ~token =
  let region = Html_assert.form_region page in
  let escaped = escape token in
  Alcotest.(check int) "exactly one name='token' field" 1
    (Html_assert.occurrences region "name='token'");
  Html_assert.must region (token_input_of escaped);
  let value = audit_token_tag (token_tag region) in
  Alcotest.(check string) "escaped value decodes back to the raw token"
    token (unescape value);
  (match List.filter Html_assert.is_csrf_input (Html_assert.input_tags region) with
  | [ _ ] -> ()
  | l ->
      Alcotest.fail
        (Printf.sprintf "expected exactly one CSRF input, found %d"
           (List.length l)));
  Alcotest.(check int) "input census: csrf + token + 2 passwords" 4
    (List.length (Html_assert.input_tags region));
  Html_assert.must page "<form action='/reset-password' method='POST' class='auth-form'>";
  Html_assert.must region
    "<input class='auth-input' type='password' id='rp-password' name='password' required minlength='8'>";
  Html_assert.must region
    "<input class='auth-input' type='password' id='rp-confirm' name='confirm_password' required minlength='8'>";
  Alcotest.(check int) "both password fields keep minlength" 2
    (Html_assert.occurrences region "minlength='8'");
  Html_assert.must region "<button type='submit' class='auth-btn'>Reset password</button>";
  Html_assert.must_not region "<script";
  Html_assert.must_not region "<img";
  Html_assert.must_not region "onerror"

(* Attribute-breakout payload: quote out of the value, then an autofocus +
   event-handler pair that would fire on load if the quote survived. *)
let breakout_payload = "x'autofocus onfocus='alert(1)"

let script_payload = "'><script>alert(1)</script>"

let single_quote_case =
  case "single-quote + event-handler payload stays inside the attribute"
    (fun () ->
      let page = render_reset breakout_payload in
      audit_reset_form page ~token:breakout_payload;
      (* The raw breakout shape must not exist anywhere in the document. *)
      Html_assert.must_not page "value='x'autofocus";
      Html_assert.must_not page "onfocus='alert";
      Html_assert.must page "value='x&#39;autofocus onfocus=&#39;alert(1)'>")

let double_quote_case =
  case "double-quote payload is entity-escaped" (fun () ->
      let token = "x\"y onmouseover=\"alert(1)" in
      let page = render_reset token in
      audit_reset_form page ~token;
      Html_assert.must_not page "\"y onmouseover=\"";
      Html_assert.must page "value='x&quot;y onmouseover=&quot;alert(1)'>")

let script_tag_case =
  case "script-tag payload never becomes an element" (fun () ->
      let page = render_reset script_payload in
      audit_reset_form page ~token:script_payload;
      Html_assert.must_not page "<script>alert";
      Html_assert.must_not page "</script>";
      Html_assert.must page "value='&#39;&gt;&lt;script&gt;alert(1)&lt;/script&gt;'>")

let amp_angle_case =
  case "ampersand and angle brackets round-trip as entities" (fun () ->
      let token = "a&b<c>d&amp;e" in
      let page = render_reset token in
      (* Double-escaping guard: the literal "&amp;e" in the token must
         render as "&amp;amp;e", and decode back to exactly one "&amp;e". *)
      audit_reset_form page ~token;
      Html_assert.must page "value='a&amp;b&lt;c&gt;d&amp;amp;e'>")

let ordinary_token_case =
  case "ordinary token is preserved byte-exact with the full form contract"
    (fun () ->
      let token = "AbC123_-xyz0" in
      let page = render_reset token in
      audit_reset_form page ~token;
      (* No escapable byte: the attribute must carry the token verbatim. *)
      Html_assert.must page (token_input_of token);
      Alcotest.(check string) "value is the raw token" token
        (audit_token_tag (token_tag (Html_assert.form_region page))))

let error_notice_of msg =
  "<div class='auth-alert auth-alert--error'>" ^ msg ^ "</div>"

(* The two real handler re-render arms. Both must keep the converted shell,
   the renderer-owned notice, and the escaped token. *)
let handler_rerender_audit ~password ~confirm ~notice () =
  let status, page =
    run_reset_post ~token:script_payload ~password ~confirm
  in
  Alcotest.(check int) "re-render status" 200 status;
  Html_assert.must page "<body class='launch-auth launch-reset-password'>";
  Html_assert.must page (error_notice_of notice);
  audit_reset_form page ~token:script_payload;
  Html_assert.must_not page "<script>alert";
  (* Submitted passwords are never echoed back. *)
  Html_assert.must_not page password

let mismatch_rerender_case =
  case "real handler: mismatch re-render escapes the crafted token"
    (handler_rerender_audit ~password:"password-one-1"
       ~confirm:"password-two-2" ~notice:"Passwords do not match.")

let too_short_rerender_case =
  case "real handler: too-short re-render escapes the crafted token"
    (handler_rerender_audit ~password:"short" ~confirm:"short"
       ~notice:"Password must be at least 8 characters.")

let wrapper_case =
  case "wrapper: launch auth shell, noindex, only local launch assets"
    (fun () ->
      let page = render_reset "wrap-check-token" in
      Html_assert.must page "<body class='launch-auth launch-reset-password'>";
      Html_assert.must page "<title>Reset Password - Earde</title>";
      Html_assert.must page "<meta name='robots' content='noindex'>";
      Html_assert.must page "<link rel='stylesheet' href='/static/css/earde.css'>";
      Alcotest.(check int) "exactly one stylesheet" 1
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "auth.css";
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "unread-notifs";
      Html_assert.must_not page "notif-badge";
      (* No script may live inside the form region regardless of the
         document-level analytics configuration other suites may leave. *)
      Html_assert.must_not (Html_assert.form_region page) "<script")

(* /login and /signup share the wrapper this pass reused; pin their form
   contracts so a regression in the shared helper or an accidental edit to
   the sibling renderers cannot slip through this suite. *)
let render_login () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered := Earde.Auth_pages.login_form req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/login" ""))
  in
  !rendered

let render_signup () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered := Earde.Auth_pages.signup_form req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/signup" ""))
  in
  !rendered

let login_signup_case =
  case "login and signup form contracts are unchanged" (fun () ->
      let login = render_login () in
      Html_assert.must login "<body class='launch-auth launch-login'>";
      Html_assert.must login "<title>Log in - Earde</title>";
      Html_assert.must login "<form action='/login' method='POST' class='auth__card'>";
      Html_assert.must login
        "<input class='input input--inset' type='text' id='li-identifier' name='identifier' required>";
      Html_assert.must login
        "<input class='input input--inset' type='password' id='li-password' name='password' required>";
      Html_assert.must login "href='/forgot-password'";
      Alcotest.(check int) "login input census" 3
        (List.length (Html_assert.input_tags (Html_assert.form_region login)));
      Html_assert.must_not login "auth.css";
      Html_assert.must_not login "tailwind";
      Html_assert.must_not login "fonts.googleapis";
      let signup = render_signup () in
      Html_assert.must signup "<body class='launch-auth launch-signup'>";
      Html_assert.must signup "<title>Create an account - Earde</title>";
      Html_assert.must signup "<form action='/signup' method='POST' class='auth__card'>";
      Html_assert.must signup "name='username' required>";
      Html_assert.must signup "name='email' required>";
      Html_assert.must signup "name='password' required>";
      Html_assert.must signup "id='website' name='website' tabindex='-1' autocomplete='off'>";
      Html_assert.must signup "id='privacy' name='privacy' type='checkbox' required>";
      Alcotest.(check int) "signup input census" 6
        (List.length (Html_assert.input_tags (Html_assert.form_region signup)));
      Html_assert.must_not signup "auth.css";
      Html_assert.must_not signup "tailwind";
      Html_assert.must_not signup "fonts.googleapis")

let suite =
  [ single_quote_case; double_quote_case; script_tag_case; amp_angle_case;
    ordinary_token_case; mismatch_rerender_case; too_short_rerender_case;
    wrapper_case; login_signup_case ]

let suites =
    (* Reset-token attribute escaping (pass 16B): the reset-password
       renderer's hidden token field escapes the attacker-controlled token
       (quote/script/entity payloads stay inert, decode losslessly, and the
       form's input census is closed), through both the direct renderer and
       the real handler's mismatch / too-short re-render arms, with the
       launch wrapper and the sibling login/signup contracts pinned.
       DB-free. *)
  [ ("reset_token_attribute_escaping", suite)
  ]
