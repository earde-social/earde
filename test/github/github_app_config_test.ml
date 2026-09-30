module GAC = Earde.Github_app_config

(* === GitHub App public configuration (Github_app_config) ===
   Pure [of_values] coverage only — the real process environment is never
   mutated. Rejection assertions compare the closed error variants, and the
   privacy cases pin that diagnostics never echo a supplied value. *)

let gac_case = Case.quick

let gac_error =
  Alcotest.testable
    (fun ppf e -> Format.pp_print_string ppf (GAC.string_of_error e))
    ( = )

let gac_err name expected result =
  gac_case name (fun () ->
      match result with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error e -> Alcotest.check gac_error "error" expected e)

let suites =
  (* Valid shapes and canonicalization: accessors must return the stored
       canonical values (trimmed, lowercase scheme/host, no trailing slash,
       no default port), not the raw environment spellings. *)
  [
    ( "github_app_config_valid",
      [
        gac_case "production https configuration" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "production"
                (Github_fixture.gac_of_values ())
            in
            Alcotest.(check string)
              "origin" "https://earde.com" (GAC.public_origin c);
            Alcotest.(check string) "slug" "earde-connect" (GAC.app_slug c);
            Alcotest.(check string)
              "client id" "Iv1.8a61f9b3a7aba766" (GAC.client_id c);
            Alcotest.(check string)
              "setup url" "https://earde.com/integrations/github/install/return"
              (GAC.setup_url c);
            Alcotest.(check string)
              "callback url"
              "https://earde.com/integrations/github/authorize/callback"
              (GAC.callback_url c));
        gac_case "localhost http with port" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "localhost"
                (Github_fixture.gac_of_values
                   ~origin:(Some "http://localhost:8080")
                   ~setup:
                     (Some
                        "http://localhost:8080/integrations/github/install/return")
                   ~callback:
                     (Some
                        "http://localhost:8080/integrations/github/authorize/callback")
                   ())
            in
            Alcotest.(check string)
              "origin" "http://localhost:8080" (GAC.public_origin c);
            Alcotest.(check string)
              "setup url"
              "http://localhost:8080/integrations/github/install/return"
              (GAC.setup_url c));
        gac_case "ipv6 loopback http with port" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "ipv6 loopback"
                (Github_fixture.gac_of_values ~origin:(Some "http://[::1]:8080")
                   ~setup:
                     (Some
                        "http://[::1]:8080/integrations/github/install/return")
                   ~callback:
                     (Some
                        "http://[::1]:8080/integrations/github/authorize/callback")
                   ())
            in
            Alcotest.(check string)
              "origin" "http://[::1]:8080" (GAC.public_origin c));
        gac_case "surrounding whitespace is trimmed" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "whitespace"
                (Github_fixture.gac_of_values
                   ~origin:(Some "  https://earde.com\n")
                   ~slug:(Some "\tearde-connect ")
                   ~client:(Some " Iv1.8a61f9b3a7aba766 ")
                   ~setup:
                     (Some
                        " https://earde.com/integrations/github/install/return ")
                   ~callback:
                     (Some
                        "\thttps://earde.com/integrations/github/authorize/callback\n")
                   ())
            in
            Alcotest.(check string)
              "origin" "https://earde.com" (GAC.public_origin c);
            Alcotest.(check string) "slug" "earde-connect" (GAC.app_slug c);
            Alcotest.(check string)
              "client id" "Iv1.8a61f9b3a7aba766" (GAC.client_id c);
            Alcotest.(check string)
              "setup url" "https://earde.com/integrations/github/install/return"
              (GAC.setup_url c));
        gac_case "origin and urls are canonicalized" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "canonicalization"
                (Github_fixture.gac_of_values
                   ~origin:(Some "HTTPS://Earde.COM:443/")
                   ~setup:
                     (Some
                        "https://earde.com:443/integrations/github/install/return")
                   ())
            in
            Alcotest.(check string)
              "origin" "https://earde.com" (GAC.public_origin c);
            Alcotest.(check string)
              "setup url" "https://earde.com/integrations/github/install/return"
              (GAC.setup_url c);
            Alcotest.(check string)
              "callback url"
              "https://earde.com/integrations/github/authorize/callback"
              (GAC.callback_url c));
      ] );
    ( "github_app_config_missing",
      [
        gac_err "missing public origin" (GAC.Missing GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:None ());
        gac_err "missing app slug" (GAC.Missing GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:None ());
        gac_err "missing client id" (GAC.Missing GAC.Client_id)
          (Github_fixture.gac_of_values ~client:None ());
        gac_err "missing setup url" (GAC.Missing GAC.Setup_url)
          (Github_fixture.gac_of_values ~setup:None ());
        gac_err "missing callback url" (GAC.Missing GAC.Callback_url)
          (Github_fixture.gac_of_values ~callback:None ());
      ] )
    (* The origin must be an absolute https (or loopback http) origin with
       nothing else on it — no userinfo, query, fragment or path. *);
    ( "github_app_config_origin",
      [
        gac_err "blank rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "   ") ());
        gac_err "relative url rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "/app") ());
        gac_err "ftp scheme rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "ftp://earde.com") ());
        gac_err "non-loopback http rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "http://earde.com") ());
        gac_err "userinfo rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values
             ~origin:(Some "https://user:pw@earde.com") ());
        gac_err "query rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "https://earde.com?x=1")
             ());
        gac_err "fragment rejected" (GAC.Invalid GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "https://earde.com#frag")
             ());
        gac_err "non-root path rejected" (GAC.Unexpected_path GAC.Public_origin)
          (Github_fixture.gac_of_values ~origin:(Some "https://earde.com/app")
             ());
      ] );
    ( "github_app_config_slug",
      [
        gac_case "representative slug accepted" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "slug"
                (Github_fixture.gac_of_values ~slug:(Some "my-app-01") ())
            in
            Alcotest.(check string) "slug" "my-app-01" (GAC.app_slug c));
        gac_err "uppercase rejected (not silently lowercased)"
          (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "My-App") ());
        gac_err "leading dash rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "-app") ());
        gac_err "trailing dash rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "app-") ());
        gac_err "slash rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "my/app") ());
        gac_err "dot rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "my.app") ());
        gac_err "interior whitespace rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "my app") ());
        gac_err "empty rejected" (GAC.Invalid GAC.App_slug)
          (Github_fixture.gac_of_values ~slug:(Some "") ());
      ] )
    (* Opaque identifier: no GitHub-specific prefix or length is imposed,
       but whitespace and control bytes are rejected. *);
    ( "github_app_config_client_id",
      [
        gac_case "unprefixed opaque id accepted" (fun () ->
            let c =
              Github_fixture.gac_ok_exn "client id"
                (Github_fixture.gac_of_values ~client:(Some "0123456789abcdef")
                   ())
            in
            Alcotest.(check string)
              "client id" "0123456789abcdef" (GAC.client_id c));
        gac_err "blank rejected" (GAC.Invalid GAC.Client_id)
          (Github_fixture.gac_of_values ~client:(Some "  ") ());
        gac_err "interior space rejected" (GAC.Invalid GAC.Client_id)
          (Github_fixture.gac_of_values ~client:(Some "Iv1. abc") ());
        gac_err "interior tab rejected" (GAC.Invalid GAC.Client_id)
          (Github_fixture.gac_of_values ~client:(Some "Iv1.\tabc") ());
        gac_err "interior newline rejected" (GAC.Invalid GAC.Client_id)
          (Github_fixture.gac_of_values ~client:(Some "Iv1.\nabc") ());
        gac_err "control character rejected" (GAC.Invalid GAC.Client_id)
          (Github_fixture.gac_of_values ~client:(Some "Iv1.\x01abc") ());
      ] )
    (* The registered setup URL must sit exactly on the public origin
       (scheme, host, effective port) and use exactly the registered
       install-return path. *);
    ( "github_app_config_setup_url",
      [
        gac_err "wrong host" (GAC.Origin_mismatch GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://evil.example/integrations/github/install/return")
             ());
        gac_err "wrong scheme" (GAC.Origin_mismatch GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:(Some "http://earde.com/integrations/github/install/return")
             ());
        gac_err "wrong effective port" (GAC.Origin_mismatch GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://earde.com:8443/integrations/github/install/return")
             ());
        gac_err "wrong path" (GAC.Unexpected_path GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:(Some "https://earde.com/integrations/github/install") ());
        gac_err "trailing slash on path" (GAC.Unexpected_path GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://earde.com/integrations/github/install/return/") ());
        gac_err "query rejected" (GAC.Invalid GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://earde.com/integrations/github/install/return?ok=1")
             ());
        gac_err "fragment rejected" (GAC.Invalid GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://earde.com/integrations/github/install/return#done")
             ());
        gac_err "userinfo rejected" (GAC.Invalid GAC.Setup_url)
          (Github_fixture.gac_of_values
             ~setup:
               (Some "https://u:p@earde.com/integrations/github/install/return")
             ());
      ] );
    ( "github_app_config_callback_url",
      [
        gac_err "wrong host" (GAC.Origin_mismatch GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some
                  "https://evil.example/integrations/github/authorize/callback")
             ());
        gac_err "wrong scheme" (GAC.Origin_mismatch GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some "http://earde.com/integrations/github/authorize/callback")
             ());
        gac_err "wrong effective port" (GAC.Origin_mismatch GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some
                  "https://earde.com:8443/integrations/github/authorize/callback")
             ());
        gac_err "setup path in callback slot"
          (GAC.Unexpected_path GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some "https://earde.com/integrations/github/install/return") ());
        gac_err "query rejected" (GAC.Invalid GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some
                  "https://earde.com/integrations/github/authorize/callback?a=b")
             ());
        gac_err "fragment rejected" (GAC.Invalid GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some
                  "https://earde.com/integrations/github/authorize/callback#x")
             ());
        gac_err "userinfo rejected" (GAC.Invalid GAC.Callback_url)
          (Github_fixture.gac_of_values
             ~callback:
               (Some
                  "https://u:p@earde.com/integrations/github/authorize/callback")
             ());
      ] )
    (* Diagnostics name the field (its env var) and reason only; the
       supplied value must never appear, so errors stay safe to log. *);
    ( "github_app_config_error_privacy",
      [
        gac_case "origin error does not echo the value" (fun () ->
            match
              Github_fixture.gac_of_values
                ~origin:(Some "ftp://private-host.internal") ()
            with
            | Ok _ -> Alcotest.fail "expected Error, got Ok"
            | Error e ->
                let msg = GAC.string_of_error e in
                Alcotest.(check bool)
                  "no supplied value" false
                  (Html_assert.contains_nonempty ~needle:"private-host" msg);
                Alcotest.(check bool)
                  "names the field" true
                  (Html_assert.contains_nonempty ~needle:"EARDE_PUBLIC_ORIGIN"
                     msg));
        gac_case "setup url error does not echo the value" (fun () ->
            match
              Github_fixture.gac_of_values
                ~setup:
                  (Some
                     "https://evil.attacker.example/integrations/github/install/return")
                ()
            with
            | Ok _ -> Alcotest.fail "expected Error, got Ok"
            | Error e ->
                let msg = GAC.string_of_error e in
                Alcotest.(check bool)
                  "no supplied value" false
                  (Html_assert.contains_nonempty ~needle:"evil.attacker.example"
                     msg);
                Alcotest.(check bool)
                  "names the field" true
                  (Html_assert.contains_nonempty ~needle:"GITHUB_APP_SETUP_URL"
                     msg));
        gac_case "client id error does not echo the value" (fun () ->
            match
              Github_fixture.gac_of_values ~client:(Some "SECRET VALUE 123") ()
            with
            | Ok _ -> Alcotest.fail "expected Error, got Ok"
            | Error e ->
                let msg = GAC.string_of_error e in
                Alcotest.(check bool)
                  "no supplied value" false
                  (Html_assert.contains_nonempty ~needle:"SECRET" msg);
                Alcotest.(check bool)
                  "names the field" true
                  (Html_assert.contains_nonempty ~needle:"GITHUB_APP_CLIENT_ID"
                     msg));
      ] );
  ]
