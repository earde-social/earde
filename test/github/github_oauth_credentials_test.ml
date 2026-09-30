module GCS = Earde.Github_oauth_credentials

(* === GitHub OAuth client secret (Github_oauth_credentials) ===
   Pure [of_values] coverage only — the real process environment is never
   mutated. All assertions on secret fixtures are boolean, so no candidate
   secret bytes reach test output on failure. *)

let gcs_case = Case.quick

let gcs_ok name value =
  gcs_case name (fun () ->
      match GCS.of_values ~client_secret:(Some value) with
      | Ok credential ->
          Alcotest.(check bool) "exact original bytes" true
            (String.equal value (GCS.client_secret credential))
      | Error _ -> Alcotest.fail "expected Ok, got Error")

let gcs_invalid name value =
  gcs_case name (fun () ->
      match GCS.of_values ~client_secret:(Some value) with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error (GCS.Invalid GCS.Client_secret) -> ()
      | Error (GCS.Missing GCS.Client_secret) ->
          Alcotest.fail "expected Invalid, got Missing")

let gcs_error_message value =
  match GCS.of_values ~client_secret:(Some value) with
  | Ok _ -> Alcotest.fail "expected Error, got Ok"
  | Error e -> GCS.string_of_error e

let suites =
    (* The client secret is opaque and byte-exact: accepted values come
       back through [client_secret] unchanged, with case and punctuation
       preserved and no trimming or repair. *)
  [ ( "github_oauth_credentials_valid"
    , [ gcs_ok "representative opaque value accepted"
          "9f8a7b6c5d4e3f2a1b0c9d8e7f6a5b4c3d2e1f0a"
      ; gcs_ok "uppercase and lowercase preserved" "AbCdEfGh0123XyZ"
      ; gcs_ok "punctuation preserved"
          "s3~!@#$%^&*()_+-=[]{}|;:'\",.<>/?`\\"
      ; gcs_case "not trimmed or normalized around punctuation" (fun () ->
            let value = "==Secret.Value==" in
            match GCS.of_values ~client_secret:(Some value) with
            | Ok credential ->
                Alcotest.(check bool) "identical bytes" true
                  (String.equal value (GCS.client_secret credential))
            | Error _ -> Alcotest.fail "expected Ok, got Error")
      ] )
  ; ( "github_oauth_credentials_missing_invalid"
    , [ gcs_case "unset value is Missing" (fun () ->
            match GCS.of_values ~client_secret:None with
            | Error (GCS.Missing GCS.Client_secret) -> ()
            | Ok _ -> Alcotest.fail "expected Error, got Ok"
            | Error (GCS.Invalid _) ->
                Alcotest.fail "expected Missing, got Invalid")
      ; gcs_invalid "empty string rejected" ""
      ; gcs_invalid "spaces only rejected" "   "
      ; gcs_invalid "leading space rejected" " abcdef"
      ; gcs_invalid "trailing space rejected" "abcdef "
      ; gcs_invalid "internal space rejected" "abc def"
      ; gcs_invalid "tab rejected" "abc\tdef"
      ; gcs_invalid "newline rejected" "abc\ndef"
      ; gcs_invalid "carriage return rejected" "abc\rdef"
      ; gcs_invalid "NUL rejected" "abc\x00def"
      ; gcs_invalid "other control byte rejected" "abc\x01def"
      ; gcs_invalid "DEL rejected" "abc\x7fdef"
      ] )
    (* No speculative format: GitHub controls the secret's shape, so no
       prefix, length or alphabet may be imposed here. *)
  ; ( "github_oauth_credentials_format"
    , [ gcs_ok "no particular prefix required" "unprefixed-secret-value"
      ; gcs_ok "single byte accepted" "x"
      ; gcs_ok "long value accepted" (String.make 200 'a')
      ; gcs_ok "non-alphanumeric bytes accepted" "a.b/c=d+e_f-g"
      ] )
    (* Diagnostics name the env var and reason only. Boolean assertions
       throughout, so a failing check never prints the fixture. *)
  ; ( "github_oauth_credentials_error_privacy"
    , [ gcs_case "field string is the env var" (fun () ->
            Alcotest.(check string) "field" "GITHUB_APP_CLIENT_SECRET"
              (GCS.string_of_field GCS.Client_secret))
      ; gcs_case "invalid message names the env var only" (fun () ->
            let value = "Distinctive$ecret With Space" in
            let msg = gcs_error_message value in
            Alcotest.(check bool) "names the env var" true
              (Html_assert.contains_nonempty ~needle:"GITHUB_APP_CLIENT_SECRET" msg);
            Alcotest.(check bool) "no full value" false
              (Html_assert.contains_nonempty ~needle:value msg);
            Alcotest.(check bool) "no distinctive substring" false
              (Html_assert.contains_nonempty ~needle:"Distinctive" msg);
            Alcotest.(check bool) "no distinctive substring 2" false
              (Html_assert.contains_nonempty ~needle:"$ecret" msg))
      ; gcs_case "missing message names the env var only" (fun () ->
            match GCS.of_values ~client_secret:None with
            | Ok _ -> Alcotest.fail "expected Error, got Ok"
            | Error e ->
                let msg = GCS.string_of_error e in
                Alcotest.(check bool) "names the env var" true
                  (Html_assert.contains_nonempty ~needle:"GITHUB_APP_CLIENT_SECRET" msg))
      ; gcs_case "no length leak" (fun () ->
            let value = "\tPrivateFixtureOfKnownLength" in
            let msg = gcs_error_message value in
            Alcotest.(check bool) "length absent" false
              (Html_assert.contains_nonempty
                 ~needle:(string_of_int (String.length value))
                 msg))
      ; gcs_case "message is value-independent (no fingerprint)" (fun () ->
            let m1 = gcs_error_message "Distinctive$ecret One " in
            let m2 = gcs_error_message "\x01entirely-other-Bytes\x7f" in
            Alcotest.(check bool) "identical for different values" true
              (String.equal m1 m2))
      ] )
  ]
