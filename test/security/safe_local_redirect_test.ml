(* === safe_local_redirect grammar (global-admin hardening) ===
   The helper reduces attacker-controlled redirect targets (the Referer
   header, form-carried return paths) to a local path+query. Browsers send
   Referer as an absolute URL, so a same-origin absolute http(s) URL must be
   accepted and reduced; everything else must collapse to the caller's
   trusted local default. Pure, DB-free. *)

let case name f = Alcotest.test_case name `Quick f

let request ?(host = Some "earde.com") () =
  Dream.request
    ~headers:(match host with Some h -> [ ("Host", h) ] | None -> [])
    ""

let check ?default ?host label target expected =
  Alcotest.(check string)
    label expected
    (Earde.Handler_support.safe_local_redirect ?default (request ?host ())
       target)

let local_paths_case =
  case "local relative paths pass; query kept; fragment dropped" (fun () ->
      check "plain path" "/admin" "/admin";
      check "root" "/" "/";
      check "query preserved" "/u/gab_target?tab=posts"
        "/u/gab_target?tab=posts";
      check "fragment dropped" "/admin#banned" "/admin";
      check "query kept, fragment dropped" "/u/bob?tab=posts#top"
        "/u/bob?tab=posts")

let same_origin_case =
  case "absolute same-origin URLs reduce to path+query" (fun () ->
      check "https referer" "https://earde.com/admin" "/admin";
      check "http referer" "http://earde.com/admin" "/admin";
      check "query preserved" "https://earde.com/u/gab_target?tab=posts"
        "/u/gab_target?tab=posts";
      check "fragment dropped" "https://earde.com/admin#sec" "/admin";
      check "explicit https port" "https://earde.com:443/admin" "/admin";
      check "explicit http port" "http://earde.com:80/admin" "/admin";
      check "empty path becomes root" "https://earde.com" "/";
      check "case-insensitive host and scheme" "HTTPS://EARDE.COM/admin"
        "/admin";
      check ~host:(Some "localhost:8080") "host with port"
        "http://localhost:8080/admin?tab=x" "/admin?tab=x")

let foreign_case =
  case "foreign origins collapse to the default" (fun () ->
      check "foreign host" "https://evil.example/admin" "/";
      check "foreign subdomain" "https://evil.earde.com/admin" "/";
      check ~default:"/admin" "foreign host, caller default"
        "https://evil.example/admin" "/admin";
      check ~host:(Some "localhost:8080") "mismatched port"
        "http://localhost:9090/admin" "/";
      check ~host:(Some "earde.com") "unexpected explicit port"
        "https://earde.com:8443/admin" "/";
      check ~host:None "no Host header to compare against"
        "https://earde.com/admin" "/")

let hostile_case =
  case "protocol-relative, userinfo, malformed and encoded tricks fall back"
    (fun () ->
      check "protocol-relative" "//evil.example" "/";
      check "protocol-relative with path" "//evil.example/admin" "/";
      check "same-origin protocol-relative path"
        "https://earde.com//evil.example" "/";
      check "userinfo trick" "https://earde.com@evil.example/admin" "/";
      check "userinfo on own host" "https://user@earde.com/admin" "/";
      check "backslash path" "/admin\\evil.example" "/";
      check "absolute with backslash" "https://earde.com/a\\b" "/";
      check "header-splitting CR LF" "/admin\r\nSet-Cookie: x=y" "/";
      check "control byte" "/admin\x00" "/";
      check "non-http scheme" "javascript:alert(1)" "/";
      check "schemeless authority" "earde.com/admin" "/";
      check "half scheme" "https:/earde.com/admin" "/";
      check "empty" "" "/";
      check ~default:"/admin" "default honored on garbage" "not a url" "/admin")

(* The output can never carry a scheme or authority, whatever comes in. *)
let never_absolute_case =
  case "no input yields a scheme, authority, or fragment" (fun () ->
      List.iter
        (fun target ->
          let out =
            Earde.Handler_support.safe_local_redirect (request ()) target
          in
          Alcotest.(check bool)
            (target ^ ": starts with single /")
            true
            (String.length out > 0
            && out.[0] = '/'
            && not (String.length out >= 2 && out.[1] = '/'));
          Alcotest.(check bool)
            (target ^ ": no scheme") false
            (Html_assert.contains out "://");
          Alcotest.(check bool)
            (target ^ ": no fragment") false (String.contains out '#'))
        [
          "/admin";
          "https://earde.com/u/x?tab=posts#f";
          "//evil.example";
          "https://evil.example/x";
          "https://earde.com//evil.example";
          "ftp://earde.com/x";
          "\\\\evil.example";
          "https://earde.com#f";
          "https://earde.com";
          "";
        ])

let suite =
  [
    local_paths_case;
    same_origin_case;
    foreign_case;
    hostile_case;
    never_absolute_case;
  ]

let suites =
  (* Global-admin ban/unban hardening: the pure redirect-target grammar
       (local paths, same-origin Referer reduction, hostile fallbacks), and
       the real POST routes over a real database — rendered form contracts,
       happy paths returning to their originating surface, the full
       CSRF-rejection matrix with zero side effects, authorization and
       target-resolution order, and the Referer grammar end to end.
       The grammar suite is DB-free; the action suite is database-gated. *)
  [ ("safe_local_redirect_grammar", suite) ]
