module GOC = Earde.Github_onboarding_crypto

(* === GitHub onboarding crypto: canonical tokens and lookup hashes ===
   Structural assertions only — generated raw state/binding values are never
   printed, and rejection cases match the payload-free [Invalid_format]
   without echoing the rejected input. *)

let goc_case = Case.quick

let goc_state_ok name input =
  goc_case name (fun () ->
      match GOC.state_of_callback input with
      | Ok state ->
          Alcotest.(check string)
            "round-trips" input
            (GOC.state_to_string state)
      | Error GOC.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let goc_state_err name input =
  goc_case name (fun () ->
      match GOC.state_of_callback input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GOC.Invalid_format -> ())

let goc_binding_ok name input =
  goc_case name (fun () ->
      match GOC.session_binding_of_string input with
      | Ok binding ->
          Alcotest.(check string)
            "round-trips" input
            (GOC.session_binding_to_string binding)
      | Error GOC.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let goc_binding_err name input =
  goc_case name (fun () ->
      match GOC.session_binding_of_string input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GOC.Invalid_format -> ())

let goc_binding_exn input =
  match GOC.session_binding_of_string input with
  | Ok binding -> binding
  | Error GOC.Invalid_format ->
      Alcotest.fail "fixture did not parse as session binding"

let goc_binding_hash input =
  GOC.session_binding_hash_to_string
    (GOC.hash_session_binding (goc_binding_exn input))

let suites =
  (* Generated tokens must already be in the canonical wire form the
       parser accepts; assertions are structural so no raw generated
       value can reach test output. *)
  [
    ( "github_crypto_generation",
      [
        goc_case "generated state is canonical 32 bytes" (fun () ->
            let encoded = GOC.state_to_string (GOC.generate_state ()) in
            Alcotest.(check bool)
              "canonical" true
              (Github_fixture.goc_canonical_32 encoded));
        goc_case "generated state re-parses" (fun () ->
            let encoded = GOC.state_to_string (GOC.generate_state ()) in
            match GOC.state_of_callback encoded with
            | Ok _ -> ()
            | Error GOC.Invalid_format ->
                Alcotest.fail "generated state rejected by parser");
        goc_case "generated binding is canonical 32 bytes" (fun () ->
            let encoded =
              GOC.session_binding_to_string (GOC.generate_session_binding ())
            in
            Alcotest.(check bool)
              "canonical" true
              (Github_fixture.goc_canonical_32 encoded));
        goc_case "generated binding re-parses" (fun () ->
            let encoded =
              GOC.session_binding_to_string (GOC.generate_session_binding ())
            in
            match GOC.session_binding_of_string encoded with
            | Ok _ -> ()
            | Error GOC.Invalid_format ->
                Alcotest.fail "generated binding rejected by parser");
      ] )
    (* Only the canonical encoding of exactly 32 bytes parses; every other
       spelling — padded, whitespace-wrapped, malformed, wrong length,
       non-zero trailing bits — is the payload-free Invalid_format. *);
    ( "github_crypto_state_parse",
      [
        goc_state_ok "canonical 32-byte fixture parses"
          (Github_fixture.goc_fixture 'A');
        goc_state_ok "all-zero canonical fixture parses"
          (Dream.to_base64url (String.make 32 '\000'));
        goc_state_err "blank rejected" "";
        goc_state_err "padded rejected" (Github_fixture.goc_fixture 'A' ^ "=");
        goc_state_err "malformed rejected" "not/base64url+data!";
        goc_state_err "leading whitespace rejected"
          (" " ^ Github_fixture.goc_fixture 'A');
        goc_state_err "trailing newline rejected"
          (Github_fixture.goc_fixture 'A' ^ "\n");
        goc_state_err "31-byte value rejected"
          (Dream.to_base64url (String.make 31 'A'));
        goc_state_err "33-byte value rejected"
          (Dream.to_base64url (String.make 33 'A'));
        goc_state_err "non-canonical trailing bits rejected"
          (String.make 42 'A' ^ "B");
      ] );
    ( "github_crypto_binding_parse",
      [
        goc_binding_ok "canonical 32-byte fixture parses"
          (Github_fixture.goc_fixture 'B');
        goc_binding_err "blank rejected" "";
        goc_binding_err "padded rejected" (Github_fixture.goc_fixture 'B' ^ "==");
        goc_binding_err "malformed rejected" "\xff\xfe not a token";
        goc_binding_err "leading whitespace rejected"
          (" " ^ Github_fixture.goc_fixture 'B');
        goc_binding_err "trailing whitespace rejected"
          (Github_fixture.goc_fixture 'B' ^ " ");
        goc_binding_err "31-byte value rejected"
          (Dream.to_base64url (String.make 31 'B'));
        goc_binding_err "33-byte value rejected"
          (Dream.to_base64url (String.make 33 'B'));
        goc_binding_err "non-canonical trailing bits rejected"
          (String.make 42 'A' ^ "C");
      ] )
    (* Hashes are deterministic 64-char lowercase hex, domain-separated
       between state and session binding, and free of raw token
       material. *);
    ( "github_crypto_hashing",
      [
        goc_case "state hashing is deterministic" (fun () ->
            Alcotest.(check string)
              "same hash"
              (Github_fixture.goc_state_hash (Github_fixture.goc_fixture 'C'))
              (Github_fixture.goc_state_hash (Github_fixture.goc_fixture 'C')));
        goc_case "binding hashing is deterministic" (fun () ->
            Alcotest.(check string)
              "same hash"
              (goc_binding_hash (Github_fixture.goc_fixture 'C'))
              (goc_binding_hash (Github_fixture.goc_fixture 'C')));
        goc_case "state hash is 64-char lowercase hex" (fun () ->
            Alcotest.(check bool)
              "hex64" true
              (Github_fixture.goc_is_hex64
                 (Github_fixture.goc_state_hash
                    (Github_fixture.goc_fixture 'D'))));
        goc_case "binding hash is 64-char lowercase hex" (fun () ->
            Alcotest.(check bool)
              "hex64" true
              (Github_fixture.goc_is_hex64
                 (goc_binding_hash (Github_fixture.goc_fixture 'D'))));
        goc_case "domains separate identical token material" (fun () ->
            Alcotest.(check bool)
              "hashes differ" false
              (String.equal
                 (Github_fixture.goc_state_hash
                    (Github_fixture.goc_fixture 'E'))
                 (goc_binding_hash (Github_fixture.goc_fixture 'E'))));
        goc_case "state hash does not contain the raw token" (fun () ->
            let token = Github_fixture.goc_fixture 'F' in
            Alcotest.(check bool)
              "no token material" false
              (Html_assert.contains_nonempty ~needle:token
                 (Github_fixture.goc_state_hash token)));
        goc_case "binding hash does not contain the raw token" (fun () ->
            let token = Github_fixture.goc_fixture 'F' in
            Alcotest.(check bool)
              "no token material" false
              (Html_assert.contains_nonempty ~needle:token
                 (goc_binding_hash token)));
      ] );
  ]
