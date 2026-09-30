(* Cartographic Civic pass 17: the shared Site_pages.msg_page through the new
   launch message wrapper. ~400 handler call sites across every status
   family render through this one document, several under byte-identity
   anti-enumeration pins, so the suite pins the wrapper contract (single
   local stylesheet, no legacy assets, no forms, no notification wiring,
   viewer- and ~auth-independence) and the renderer contract (title and
   message always escaped — markup supplied in either stays inert text —
   the alert_type glyph mapping, and the verbatim return_url Go back
   link). Handlers stay authoritative for status and headers; the gated
   suites keep pinning those and the byte-identity pairs. *)

let case name f = Alcotest.test_case name `Quick f

let render ?user ?auth ?(title = "Not Found")
    ?(message = "This page does not exist.") ?(alert_type = "error")
    ?(return_url = "/") () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered :=
             Earde.Site_pages.msg_page ?user ?auth ~title ~message ~alert_type
               ~return_url req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/qa-msg" ""))
  in
  !rendered

let wrapper_case =
  case "wrapper: neutral launch message document, only local assets"
    (fun () ->
      let page = render () in
      Html_assert.must page "<body class='launch-message-page'>";
      Html_assert.must page "<title>Not Found - Earde</title>";
      Html_assert.must page "<link rel='stylesheet' href='/static/css/earde.css'>";
      Alcotest.(check int) "exactly one stylesheet" 1
        (Html_assert.occurrences page "<link rel='stylesheet'");
      Html_assert.must_not page "tailwind";
      Html_assert.must_not page "fonts.googleapis";
      Html_assert.must_not page "auth.css";
      Html_assert.must_not page "shell.css";
      Html_assert.must_not page "mobile-gate.css";
      Html_assert.must_not page "unread-notifs";
      Html_assert.must_not page "notif-badge";
      Html_assert.must_not page "copyPostLink";
      Html_assert.must_not page "confirmModal";
      Html_assert.must_not page "noindex";
      Html_assert.must_not page "href='#'";
      (* No chrome that could vary by resource or viewer. *)
      Html_assert.must_not page "rail__item";
      Html_assert.must_not page "topbar";
      (* The document introduces no forms and (analytics unconfigured in
         tests) no ids at all — trivially no duplicates. *)
      Html_assert.must_not page "<form";
      Alcotest.(check int) "no ids" 0 (Html_assert.occurrences page " id='"))

(* Both historical ~auth branches collapse into the same document family —
   byte-identical, since they already were under the legacy wrapper. *)
let auth_branch_parity_case =
  case "~auth:true and ~auth:false render byte-identically" (fun () ->
      let a = render ~auth:true () and b = render ~auth:false () in
      Html_assert.must a "<body class='launch-message-page'>";
      Alcotest.(check string) "auth branches identical" a b)

let viewer_independence_case =
  case "ignored ?user changes nothing" (fun () ->
      Alcotest.(check string) "anonymous = authenticated" (render ())
        (render ~user:"qa-viewer" ()))

(* The always-escaped-text contract: no caller-supplied markup in title or
   message may reach the document unescaped (msg_page has never accepted
   trusted HTML). *)
let escaping_case =
  case "title and message stay escaped text" (fun () ->
      let page =
        render ~title:"Alert <\"quoted\"> & 'ticked'"
          ~message:"See <a href='/x'>link</a> & <script>alert(1)</script>" ()
      in
      Html_assert.must page
        "<h1 class='auth__title'>Alert &lt;&quot;quoted&quot;&gt; &amp; \
         &#39;ticked&#39;</h1>";
      Html_assert.must page
        "<title>Alert &lt;&quot;quoted&quot;&gt; &amp; &#39;ticked&#39; - \
         Earde</title>";
      Html_assert.must page
        "See &lt;a href=&#39;/x&#39;&gt;link&lt;/a&gt; &amp; \
         &lt;script&gt;alert(1)&lt;/script&gt;";
      Html_assert.must_not page "<a href='/x'>";
      Html_assert.must_not page "<script>alert")

(* The renderer's own return-to-context link: verbatim caller URL, one
   link, unchanged label. *)
let go_back_case =
  case "return_url renders as the single Go back link" (fun () ->
      let page = render ~return_url:"/c/qa-somewhere/settings" () in
      Html_assert.must page
        "<a href='/c/qa-somewhere/settings' class='launch-msg__back'>Go \
         back</a>";
      Alcotest.(check int) "exactly one Go back" 1 (Html_assert.occurrences page "Go back"))

(* alert_type keeps its historical mapping: success / info / everything
   else (including unknown values) is the error glyph. *)
let alert_type_case =
  case "alert_type maps onto the three glyph variants" (fun () ->
      List.iter
        (fun (alert_type, variant) ->
          let page = render ~alert_type () in
          Html_assert.must page ("launch-msg__icon launch-msg__icon--" ^ variant);
          Alcotest.(check int) (alert_type ^ ": one glyph") 1
            (Html_assert.occurrences page "launch-msg__icon "))
        [ ("success", "success"); ("info", "info"); ("error", "error");
          ("banana", "error") ])

(* The wrapper must not introduce the substrings the project-home 409
   neutrality tests forbid, nor any resource-derived copy. *)
let neutral_copy_case =
  case "wrapper adds no forbidden or resource-derived copy" (fun () ->
      let page =
        String.lowercase_ascii
          (render ~title:"Not Allowed"
             ~message:"This request is not allowed." ())
      in
      Html_assert.must_not page "private";
      Html_assert.must_not page "draft";
      Html_assert.must_not page "legacy")

let suite =
  [ wrapper_case; auth_branch_parity_case; viewer_independence_case;
    escaping_case; go_back_case; alert_type_case; neutral_copy_case ]

let suites =
  [ ("launch_message_page", suite)
  ]
