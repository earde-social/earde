module GPK = Earde.Github_onboarding_pkce
module GAC = Earde.Github_app_config
module GOU = Earde.Github_onboarding_urls

(* === GitHub onboarding URLs (Github_onboarding_urls) ===
   Structural assertions over [Uri.of_string]-parsed output. The fixtures
   are deterministic printable values, but state and challenge comparisons
   still use boolean checks so no raw token material reaches test output. *)

let gou_case = Case.quick

let gou_config () = Github_fixture.gac_ok_exn "url fixture config" (Github_fixture.gac_of_values ())

let gou_state_string = Github_fixture.goc_fixture 'S'

let gou_state () = Github_fixture.goc_state_exn gou_state_string

(* The verifier string is retained so leakage tests can assert it never
   appears in a URL; only its derived challenge may. *)
let gou_verifier_string = Github_fixture.goc_fixture 'V'

let gou_challenge () =
  GPK.challenge_of_verifier (Github_fixture.gpk_verifier_exn gou_verifier_string)

let gou_installation () =
  GOU.installation_url (gou_config ()) ~state:(gou_state ())

let gou_authorization ?config () =
  let config = match config with Some c -> c | None -> gou_config () in
  GOU.authorization_url config ~state:(gou_state ())
    ~code_challenge:(gou_challenge ())

let suites =
    (* Installation URL: fixed GitHub origin, slug-derived path, and
       exactly one query parameter — the raw one-time state. *)
  [ ( "github_onboarding_urls_installation"
    , [ gou_case "scheme and host are fixed" (fun () ->
            let uri = Uri.of_string (gou_installation ()) in
            Alcotest.(check (option string)) "scheme" (Some "https")
              (Uri.scheme uri);
            Alcotest.(check (option string)) "host" (Some "github.com")
              (Uri.host uri))
      ; gou_case "path is the slug installation path" (fun () ->
            let uri = Uri.of_string (gou_installation ()) in
            Alcotest.(check string) "path"
              "/apps/earde-connect/installations/new" (Uri.path uri))
      ; gou_case "query is exactly one state" (fun () ->
            let uri = Uri.of_string (gou_installation ()) in
            Alcotest.(check (list string)) "keys" [ "state" ]
              (Github_fixture.gou_keys uri);
            Alcotest.(check bool) "state round-trips" true
              (String.equal gou_state_string
                 (Github_fixture.gou_single "installation" "state" uri)))
      ; gou_case "no fragment or userinfo" (fun () ->
            let uri = Uri.of_string (gou_installation ()) in
            Alcotest.(check (option string)) "fragment" None
              (Uri.fragment uri);
            Alcotest.(check (option string)) "userinfo" None
              (Uri.userinfo uri))
      ; gou_case "state is not in the path" (fun () ->
            let uri = Uri.of_string (gou_installation ()) in
            Alcotest.(check bool) "path free of state" false
              (Html_assert.contains_nonempty ~needle:gou_state_string (Uri.path uri)))
      ; gou_case "construction is deterministic" (fun () ->
            Alcotest.(check bool) "equal urls" true
              (String.equal (gou_installation ()) (gou_installation ())))
      ] )
    (* Authorization URL: fixed GitHub origin, the exact five OAuth+PKCE
       parameters once each, and the registered callback emitted
       verbatim. *)
  ; ( "github_onboarding_urls_authorization"
    , [ gou_case "scheme and host are fixed" (fun () ->
            let uri = Uri.of_string (gou_authorization ()) in
            Alcotest.(check (option string)) "scheme" (Some "https")
              (Uri.scheme uri);
            Alcotest.(check (option string)) "host" (Some "github.com")
              (Uri.host uri))
      ; gou_case "path is the authorize endpoint" (fun () ->
            let uri = Uri.of_string (gou_authorization ()) in
            Alcotest.(check string) "path" "/login/oauth/authorize"
              (Uri.path uri))
      ; gou_case "query keys are exactly the five, once each" (fun () ->
            let uri = Uri.of_string (gou_authorization ()) in
            Alcotest.(check (list string)) "keys" Github_fixture.gou_authorization_keys
              (Github_fixture.gou_keys uri))
      ; gou_case "values match their sources" (fun () ->
            let config = gou_config () in
            let uri = Uri.of_string (gou_authorization ~config ()) in
            Alcotest.(check string) "client_id" (GAC.client_id config)
              (Github_fixture.gou_single "authorization" "client_id" uri);
            Alcotest.(check string) "redirect_uri"
              "https://earde.com/integrations/github/authorize/callback"
              (Github_fixture.gou_single "authorization" "redirect_uri" uri);
            Alcotest.(check bool) "state round-trips" true
              (String.equal gou_state_string
                 (Github_fixture.gou_single "authorization" "state" uri));
            Alcotest.(check bool) "challenge round-trips" true
              (String.equal
                 (GPK.challenge_to_string (gou_challenge ()))
                 (Github_fixture.gou_single "authorization" "code_challenge" uri));
            Alcotest.(check string) "method" "S256"
              (Github_fixture.gou_single "authorization" "code_challenge_method" uri))
      ; gou_case "no forbidden parameters" (fun () ->
            let uri = Uri.of_string (gou_authorization ()) in
            List.iter
              (fun key ->
                Alcotest.(check int) key 0
                  (List.length (Github_fixture.gou_entries key uri)))
              [ "scope"; "client_secret"; "code_verifier";
                "installation_id"; "allow_signup"; "login" ])
      ; gou_case "no setup URL" (fun () ->
            let config = gou_config () in
            let url = gou_authorization ~config () in
            let values =
              List.concat_map snd (Uri.query (Uri.of_string url))
            in
            Alcotest.(check bool) "no setup value" false
              (List.exists (String.equal (GAC.setup_url config)) values);
            Alcotest.(check bool) "no setup path" false
              (Html_assert.contains_nonempty ~needle:"install/return" url))
      ; gou_case "no fragment or userinfo" (fun () ->
            let uri = Uri.of_string (gou_authorization ()) in
            Alcotest.(check (option string)) "fragment" None
              (Uri.fragment uri);
            Alcotest.(check (option string)) "userinfo" None
              (Uri.userinfo uri))
      ; gou_case "construction is deterministic" (fun () ->
            Alcotest.(check bool) "equal urls" true
              (String.equal (gou_authorization ()) (gou_authorization ())))
      ] )
    (* Reserved characters in opaque values must be query-encoded, so a
       hostile-looking client id can neither split the query nor smuggle
       extra parameters. *)
  ; ( "github_onboarding_urls_encoding"
    , [ gou_case "client id with reserved characters round-trips" (fun () ->
            let client = "Iv1.a&b=c+d?e" in
            let config =
              Github_fixture.gac_ok_exn "reserved client id"
                (Github_fixture.gac_of_values ~client:(Some client) ())
            in
            let uri = Uri.of_string (gou_authorization ~config ()) in
            Alcotest.(check string) "client_id" client
              (Github_fixture.gou_single "encoding" "client_id" uri);
            Alcotest.(check (list string)) "keys unchanged"
              Github_fixture.gou_authorization_keys (Github_fixture.gou_keys uri))
      ] )
    (* Nothing secret-shaped may appear in either URL: no verifier, no
       secret-marker parameter names, and the installation URL carries no
       OAuth material at all. *)
  ; ( "github_onboarding_urls_no_leakage"
    , [ gou_case "verifier absent from authorization URL" (fun () ->
            Alcotest.(check bool) "verifier absent" false
              (Html_assert.contains_nonempty ~needle:gou_verifier_string
                 (gou_authorization ())))
      ; gou_case "no secret-marker names in either URL" (fun () ->
            List.iter
              (fun url ->
                List.iter
                  (fun needle ->
                    Alcotest.(check bool) needle false
                      (Html_assert.contains_nonempty ~needle url))
                  [ "client_secret"; "code_verifier"; "private_key";
                    "access_token" ])
              [ gou_installation (); gou_authorization () ])
      ; gou_case "installation URL has no client id or callback" (fun () ->
            let config = gou_config () in
            let url = gou_installation () in
            Alcotest.(check bool) "no client id" false
              (Html_assert.contains_nonempty ~needle:(GAC.client_id config) url);
            Alcotest.(check bool) "no callback url" false
              (Html_assert.contains_nonempty ~needle:(GAC.callback_url config) url);
            Alcotest.(check bool) "no callback path" false
              (Html_assert.contains_nonempty ~needle:"authorize/callback" url))
      ] )
  ]
