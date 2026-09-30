(* The Html boundary: escaping by construction for text and attribute
   values, URL policies by context, literal-only markup, and the census that
   keeps the one escape hatch rare. Pure; no request and no database. *)

module H = Earde.Html

let case = Case.quick
let render = H.to_string

let text_case =
  case "text: the five markup characters are entities, nothing else changes"
    (fun () ->
      Alcotest.(check string)
        "specials" "&lt;&gt;&amp;&quot;&#39;"
        (render (H.text "<>&\"'"));
      Alcotest.(check string)
        "plain" "ordinary text 123"
        (render (H.text "ordinary text 123"));
      Alcotest.(check string)
        "no double decoding" "&amp;lt;"
        (render (H.text "&lt;")))

let attribute_breakout_case =
  case "template: hostile text cannot leave its attribute or element" (fun () ->
      let out =
        render
          (H.template "<a title='%s' data-x=\"%s\">%s</a>"
             [
               H.text "' onmouseover='alert(1)";
               H.text "\" onfocus=\"alert(1)";
               H.text "</a><script>alert(1)</script>";
             ])
      in
      Alcotest.(check string)
        "escaped in place"
        "<a title='&#39; onmouseover=&#39;alert(1)' data-x=\"&quot; \
         onfocus=&quot;alert(1)\">&lt;/a&gt;&lt;script&gt;alert(1)&lt;/script&gt;</a>"
        out)

let template_case =
  case "template: holes are filled once, in order, and must match exactly"
    (fun () ->
      Alcotest.(check string)
        "percent is literal" "<p>100% of a</p>"
        (render (H.template "<p>100% of %s</p>" [ H.text "a" ]));
      Alcotest.(check string)
        "a hole value is not re-expanded" "<p>%s and b</p>"
        (render (H.template "<p>%s and %s</p>" [ H.text "%s"; H.text "b" ]));
      Alcotest.check_raises "too few holes"
        (Invalid_argument "Html.template: more markers than holes") (fun () ->
          ignore (H.template "<p>%s %s</p>" [ H.text "a" ]));
      Alcotest.check_raises "too many holes"
        (Invalid_argument "Html.template: more holes than markers") (fun () ->
          ignore (H.template "<p>%s</p>" [ H.text "a"; H.text "b" ])))

let external_url_case =
  case "external_url: http(s) only, escaped; everything else is inert"
    (fun () ->
      Alcotest.(check string)
        "https" "https://example.org/a?b=1&amp;c=2"
        (render (H.external_url "https://example.org/a?b=1&c=2"));
      Alcotest.(check string)
        "case-insensitive scheme" "HTTP://example.org"
        (render (H.external_url "HTTP://example.org"));
      Alcotest.(check string)
        "quote cannot close the attribute"
        "https://x.example/&#39;onmouseover=&#39;a"
        (render (H.external_url "https://x.example/'onmouseover='a"));
      List.iter
        (fun hostile ->
          Alcotest.(check string) hostile "#" (render (H.external_url hostile)))
        [
          "javascript:alert(1)";
          "JaVaScRiPt:alert(1)";
          " javascript:alert(1)";
          "data:text/html,<script>alert(1)</script>";
          "//evil.example/x";
          "/relative/path";
          "";
          "vbscript:msgbox(1)";
        ])

let internal_path_case =
  case "internal_path: rooted paths only; never protocol-relative" (fun () ->
      Alcotest.(check string) "root" "/" (render (H.internal_path "/"));
      Alcotest.(check string)
        "path" "/c/example/t/12-a-thread"
        (render (H.internal_path "/c/example/t/12-a-thread"));
      Alcotest.(check string)
        "escaped" "/x&#39;&gt;&lt;svg"
        (render (H.internal_path "/x'><svg"));
      List.iter
        (fun hostile ->
          Alcotest.(check string) hostile "#" (render (H.internal_path hostile)))
        [
          "//evil.example";
          "/\\evil.example";
          "javascript:alert(1)";
          "https://evil.example";
          "";
          "#";
          "c/example";
        ];
      Alcotest.(check bool)
        "opt refuses" true
        (H.internal_path_opt "//evil.example" = None))

let image_src_case =
  case "image_src: local uploads and http(s); hostile candidates refused"
    (fun () ->
      Alcotest.(check string)
        "upload" "/static/uploads/earde_1_2.webp"
        (render (H.image_src "/static/uploads/earde_1_2.webp"));
      Alcotest.(check string)
        "trimmed https" "https://cdn.example/a.png"
        (render (H.image_src "  https://cdn.example/a.png  "));
      List.iter
        (fun hostile ->
          Alcotest.(check string) hostile "#" (render (H.image_src hostile)))
        [
          "javascript:alert(1)";
          "data:image/png;base64,AAAA";
          "/a.png' onerror='alert(1)";
          "/a.png\" onerror=\"alert(1)";
          "/a b.png";
          "/a.png`";
          "//cdn.example/a.png";
          "/\\cdn.example";
          "";
          "   ";
        ];
      Alcotest.(check bool)
        "opt refuses" true
        (H.image_src_opt "javascript:alert(1)" = None))

let composition_case =
  case "composition: concat, join and emptiness" (fun () ->
      let open H.Infix in
      Alcotest.(check string)
        "concat" "<b>a</b>&amp;"
        (render (H.concat [ H.static "<b>a</b>"; H.text "&" ]));
      Alcotest.(check string)
        "join" "a, b"
        (render (H.join (H.static ", ") [ H.text "a"; H.text "b" ]));
      Alcotest.(check string) "infix" "ab" (render (H.text "a" ++ H.text "b"));
      Alcotest.(check bool) "empty" true (H.is_empty H.empty);
      Alcotest.(check bool) "not empty" false (H.is_empty (H.text "x")))

(* --- census --------------------------------------------------------- *)

(* Source text with comments and string contents blanked, so prose or a
   message mentioning the API is not read as a call. Strings inside comments
   are skipped as OCaml lexes them. *)
let code_of src =
  let b = Bytes.of_string src and n = String.length src in
  let rec skip_string i =
    if i >= n then i
    else if src.[i] = '\\' then skip_string (i + 2)
    else if src.[i] = '"' then i + 1
    else skip_string (i + 1)
  in
  let rec comment i depth =
    if i >= n then i
    else if i + 1 < n && src.[i] = '(' && src.[i + 1] = '*' then (
      Bytes.fill b i 2 ' ';
      comment (i + 2) (depth + 1))
    else if i + 1 < n && src.[i] = '*' && src.[i + 1] = ')' then (
      Bytes.fill b i 2 ' ';
      if depth = 1 then i + 2 else comment (i + 2) (depth - 1))
    else if src.[i] = '"' then (
      let j = skip_string (i + 1) in
      Bytes.fill b i (j - i) ' ';
      comment j depth)
    else (
      if src.[i] <> '\n' then Bytes.set b i ' ';
      comment (i + 1) depth)
  in
  let rec go i =
    if i >= n then ()
    else if i + 1 < n && src.[i] = '(' && src.[i + 1] = '*' then
      go (comment i 0)
    else if src.[i] = '"' then (
      (* keep the quotes, so a literal is still visible as one *)
      let j = skip_string (i + 1) in
      if j - i > 2 then Bytes.fill b (i + 1) (j - i - 2) ' ';
      go j)
    else go (i + 1)
  in
  go 0;
  Bytes.to_string b

let calls needle code =
  let nl = String.length needle in
  let rec go i acc =
    match Html_assert.index_from code needle i with
    | None -> List.rev acc
    | Some j -> go (j + nl) ((j + nl) :: acc)
  in
  go 0 []

let literal_follows code i =
  let n = String.length code in
  let rec skip i =
    if i < n && (code.[i] = ' ' || code.[i] = '\n' || code.[i] = '(') then
      skip (i + 1)
    else i
  in
  let i = skip i in
  i < n && (code.[i] = '"' || code.[i] = '{')

let lib_implementations () =
  List.filter
    (fun (path, _) -> String.length path > 4 && String.sub path 0 4 = "lib/")
    Source_census.implementations

let literal_markup_case =
  case "census: markup enters only as a literal" (fun () ->
      List.iter
        (fun (path, src) ->
          let code = code_of src in
          List.iter
            (fun needle ->
              List.iter
                (fun i ->
                  if not (literal_follows code i) then
                    Alcotest.failf "%s: %s without a string literal" path needle)
                (calls needle code))
            [ "Html.static"; "Html.template" ])
        (lib_implementations ()))

let trusted_census_case =
  case "census: the one escape hatch is the CSRF field" (fun () ->
      let users =
        List.filter
          (fun (_, src) -> calls "Html.trusted" (code_of src) <> [])
          (lib_implementations ())
      in
      Alcotest.(check (list string))
        "Html.trusted call sites" [ "lib/csrf_field.ml" ] (List.map fst users))

let suites =
  [
    ( "html_boundary",
      [
        text_case;
        attribute_breakout_case;
        template_case;
        external_url_case;
        internal_path_case;
        image_src_case;
        composition_case;
      ] );
    ("html_census", [ literal_markup_case; trusted_census_case ]);
  ]
