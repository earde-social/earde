module GPK = Earde.Github_onboarding_pkce

(* === GitHub onboarding PKCE: verifier and S256 challenge ===
   Same discipline as the GOC suites: structural assertions for generated
   material (never printed), fixed printable fixtures for parse/challenge
   cases, payload-free Invalid_format on rejection. Challenge equality
   checks use fixed verifiers, so determinism is proven directly rather
   than probabilistically. *)

let gpk_case = Case.quick

let gpk_verifier_ok name input =
  gpk_case name (fun () ->
      match GPK.verifier_of_string input with
      | Ok verifier ->
          Alcotest.(check string)
            "round-trips" input
            (GPK.verifier_to_string verifier)
      | Error GPK.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let gpk_verifier_err name input =
  gpk_case name (fun () ->
      match GPK.verifier_of_string input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GPK.Invalid_format -> ())

let gpk_challenge input =
  GPK.challenge_to_string
    (GPK.challenge_of_verifier (Github_fixture.gpk_verifier_exn input))

let suites =
  (* Generated verifiers must already be in the canonical wire form the
       parser accepts; assertions are structural so no generated value can
       reach test output. *)
  [
    ( "github_pkce_generation",
      [
        gpk_case "generated verifier is 43 chars" (fun () ->
            let encoded = GPK.verifier_to_string (GPK.generate_verifier ()) in
            Alcotest.(check int) "length" 43 (String.length encoded));
        gpk_case "generated verifier decodes to 32 bytes" (fun () ->
            let encoded = GPK.verifier_to_string (GPK.generate_verifier ()) in
            match Dream.from_base64url encoded with
            | Some raw ->
                Alcotest.(check int) "decoded bytes" 32 (String.length raw)
            | None -> Alcotest.fail "generated verifier does not decode");
        gpk_case "generated verifier has no padding" (fun () ->
            let encoded = GPK.verifier_to_string (GPK.generate_verifier ()) in
            Alcotest.(check bool) "no '='" false (String.contains encoded '='));
        gpk_case "generated verifier re-parses and round-trips" (fun () ->
            let encoded = GPK.verifier_to_string (GPK.generate_verifier ()) in
            match GPK.verifier_of_string encoded with
            | Ok reparsed ->
                Alcotest.(check bool)
                  "round-trips exactly" true
                  (String.equal encoded (GPK.verifier_to_string reparsed))
            | Error GPK.Invalid_format ->
                Alcotest.fail "generated verifier rejected by parser");
      ] )
    (* Only the canonical encoding of exactly 32 bytes parses; every other
       spelling — padded, whitespace-wrapped, malformed, wrong length,
       non-zero trailing bits — is the payload-free Invalid_format. *);
    ( "github_pkce_verifier_parse",
      [
        gpk_verifier_ok "canonical 32-byte fixture parses"
          (Github_fixture.goc_fixture 'P');
        gpk_verifier_err "blank rejected" "";
        gpk_verifier_err "malformed rejected" "not/base64url+data!";
        gpk_verifier_err "padded rejected" (Github_fixture.goc_fixture 'P' ^ "=");
        gpk_verifier_err "leading whitespace rejected"
          (" " ^ Github_fixture.goc_fixture 'P');
        gpk_verifier_err "trailing whitespace rejected"
          (Github_fixture.goc_fixture 'P' ^ " ");
        gpk_verifier_err "31-byte value rejected"
          (Dream.to_base64url (String.make 31 'P'));
        gpk_verifier_err "33-byte value rejected"
          (Dream.to_base64url (String.make 33 'P'));
        gpk_verifier_err "non-canonical trailing bits rejected"
          (String.make 42 'A' ^ "B");
      ] )
    (* S256 challenges: deterministic canonical 43-char Base64url of the
       raw 32-byte digest, pinned to the RFC 7636 appendix-B vector so the
       exact construction (string hashed, no hex, no separator) cannot
       silently drift. *);
    ( "github_pkce_challenge",
      [
        gpk_case "same verifier gives the same challenge" (fun () ->
            Alcotest.(check string)
              "deterministic"
              (gpk_challenge (Github_fixture.goc_fixture 'Q'))
              (gpk_challenge (Github_fixture.goc_fixture 'Q')));
        gpk_case "challenge is 43 chars" (fun () ->
            Alcotest.(check int)
              "length" 43
              (String.length (gpk_challenge (Github_fixture.goc_fixture 'Q'))));
        gpk_case "challenge has no padding" (fun () ->
            Alcotest.(check bool)
              "no '='" false
              (String.contains
                 (gpk_challenge (Github_fixture.goc_fixture 'Q'))
                 '='));
        gpk_case "challenge is canonical Base64url of 32 bytes" (fun () ->
            Alcotest.(check bool)
              "canonical" true
              (Github_fixture.goc_canonical_32
                 (gpk_challenge (Github_fixture.goc_fixture 'Q'))));
        gpk_case "different verifiers give different challenges" (fun () ->
            Alcotest.(check bool)
              "challenges differ" false
              (String.equal
                 (gpk_challenge (Github_fixture.goc_fixture 'Q'))
                 (gpk_challenge (Github_fixture.goc_fixture 'R'))));
        gpk_case "RFC 7636 S256 reference vector" (fun () ->
            Alcotest.(check string)
              "challenge" "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
              (gpk_challenge "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"));
      ] );
  ]
