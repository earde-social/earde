module GPK = Earde.Github_onboarding_pkce
module GSD = Earde.Github_onboarding_session_data

(* === GitHub onboarding session data (Github_onboarding_session_data) ===
   Private per-flow browser material: one encrypted cookie per onboarding
   state (encryption is the future Dream cookie adapter's job — the module
   only supplies plaintext values and deterministic cookie names). Same
   discipline as the GOC/GPK suites: structural assertions for generated
   material — nothing generated (binding, verifier, challenge, or their
   hashes) is ever printed — and boolean equality checks so assertion
   failures expose no secrets. Rejection cases match the payload-free
   [Invalid_format] without echoing the rejected input. Cookie-name cases
   use fixed printable states, so determinism is proven directly. *)

let gsd_case = Case.quick

let gsd_decode_err name input =
  gsd_case name (fun () ->
      match GSD.decode input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GSD.Invalid_format -> ())

let gsd_decode_exn label input =
  match GSD.decode input with
  | Ok data -> data
  | Error GSD.Invalid_format -> Alcotest.failf "%s: decode rejected" label

(* Valid canonical components for building malformed encodings; fixtures
   are deterministic and printable, unlike generated material. *)
let gsd_fixture_binding = Github_fixture.goc_fixture 'B'
let gsd_fixture_verifier = Github_fixture.goc_fixture 'C'

let gsd_cookie_of fixture =
  GSD.cookie_name (Github_fixture.goc_state_exn fixture)

let suites =
  (* Created material must be structurally canonical, with binding and
       verifier generated independently and the challenge derived (not
       stored). Assertions are boolean so no generated value can reach
       test output. *)
  [
    ( "github_session_data_creation",
      [
        gsd_case "binding hash is 64-char lowercase hex" (fun () ->
            Alcotest.(check bool)
              "hex64" true
              (Github_fixture.goc_is_hex64
                 (Github_fixture.gsd_binding_hash (GSD.create ()))));
        gsd_case "verifier is canonical 43 chars" (fun () ->
            let verifier = Github_fixture.gsd_verifier (GSD.create ()) in
            Alcotest.(check int) "length" 43 (String.length verifier);
            Alcotest.(check bool)
              "canonical" true
              (Github_fixture.goc_canonical_32 verifier));
        gsd_case "challenge is canonical 43 chars" (fun () ->
            let challenge = Github_fixture.gsd_challenge (GSD.create ()) in
            Alcotest.(check int) "length" 43 (String.length challenge);
            Alcotest.(check bool)
              "canonical" true
              (Github_fixture.goc_canonical_32 challenge));
        gsd_case "challenge equals derivation from returned verifier" (fun () ->
            let data = GSD.create () in
            Alcotest.(check bool)
              "same challenge" true
              (String.equal
                 (Github_fixture.gsd_challenge data)
                 (GPK.challenge_to_string
                    (GPK.challenge_of_verifier (GSD.verifier data)))));
      ] )
    (* The versioned encoding round-trips exactly: decode accepts encode's
       output, re-encoding is byte-identical, and the decoded value carries
       the same binding hash, verifier, and derived challenge. *);
    ( "github_session_data_roundtrip",
      [
        gsd_case "encode/decode/re-encode is byte-identical" (fun () ->
            let encoded = GSD.encode (GSD.create ()) in
            let decoded = gsd_decode_exn "roundtrip" encoded in
            Alcotest.(check bool)
              "re-encodes exactly" true
              (String.equal encoded (GSD.encode decoded)));
        gsd_case "decoded material matches the original" (fun () ->
            let data = GSD.create () in
            let decoded = gsd_decode_exn "material" (GSD.encode data) in
            Alcotest.(check bool)
              "same verifier" true
              (String.equal
                 (Github_fixture.gsd_verifier data)
                 (Github_fixture.gsd_verifier decoded));
            Alcotest.(check bool)
              "same binding hash" true
              (String.equal
                 (Github_fixture.gsd_binding_hash data)
                 (Github_fixture.gsd_binding_hash decoded));
            Alcotest.(check bool)
              "same challenge" true
              (String.equal
                 (Github_fixture.gsd_challenge data)
                 (Github_fixture.gsd_challenge decoded)));
        gsd_case "encoding has exactly three components" (fun () ->
            Alcotest.(check int)
              "components" 3
              (List.length
                 (String.split_on_char '.' (GSD.encode (GSD.create ())))));
        gsd_case "encoding begins with v1." (fun () ->
            let encoded = GSD.encode (GSD.create ()) in
            Alcotest.(check bool)
              "v1. prefix" true
              (String.length encoded > 3
              && String.equal (String.sub encoded 0 3) "v1."));
      ] )
    (* Only the exact [v1.<binding>.<verifier>] spelling parses; every
       other shape — reshuffled components, padded or non-canonical
       tokens, whitespace — is the payload-free Invalid_format. *);
    ( "github_session_data_decode_rejection",
      [
        gsd_decode_err "blank rejected" "";
        gsd_decode_err "unknown version rejected"
          ("v2." ^ gsd_fixture_binding ^ "." ^ gsd_fixture_verifier);
        gsd_decode_err "missing binding rejected" ("v1." ^ gsd_fixture_verifier);
        gsd_decode_err "missing verifier rejected" ("v1." ^ gsd_fixture_binding);
        gsd_decode_err "extra component rejected"
          ("v1." ^ gsd_fixture_binding ^ "." ^ gsd_fixture_verifier ^ "."
         ^ gsd_fixture_verifier);
        gsd_decode_err "empty binding component rejected"
          ("v1.." ^ gsd_fixture_verifier);
        gsd_decode_err "empty verifier component rejected"
          ("v1." ^ gsd_fixture_binding ^ ".");
        gsd_decode_err "malformed binding rejected"
          ("v1.not/base64url+data!." ^ gsd_fixture_verifier);
        gsd_decode_err "malformed verifier rejected"
          ("v1." ^ gsd_fixture_binding ^ ".not/base64url+data!");
        gsd_decode_err "padded binding rejected"
          ("v1." ^ gsd_fixture_binding ^ "=." ^ gsd_fixture_verifier);
        gsd_decode_err "padded verifier rejected"
          ("v1." ^ gsd_fixture_binding ^ "." ^ gsd_fixture_verifier ^ "=");
        gsd_decode_err "leading whitespace rejected"
          (" v1." ^ gsd_fixture_binding ^ "." ^ gsd_fixture_verifier);
        gsd_decode_err "trailing whitespace rejected"
          ("v1." ^ gsd_fixture_binding ^ "." ^ gsd_fixture_verifier ^ " ");
        gsd_decode_err "non-canonical binding spelling rejected"
          ("v1." ^ String.make 42 'A' ^ "B." ^ gsd_fixture_verifier);
        gsd_decode_err "non-canonical verifier spelling rejected"
          ("v1." ^ gsd_fixture_binding ^ "." ^ String.make 42 'A' ^ "B");
      ] )
    (* Cookie names are exactly prefix + full state hash: deterministic
       per state, distinct across states, and free of raw state, secret
       material, whitespace, and control characters. *);
    ( "github_session_data_cookie_names",
      [
        gsd_case "name is exactly prefix plus state hash" (fun () ->
            Alcotest.(check string)
              "exact name"
              (Github_fixture.gsd_cookie_prefix
              ^ Github_fixture.goc_state_hash (Github_fixture.goc_fixture 'S'))
              (gsd_cookie_of (Github_fixture.goc_fixture 'S')));
        gsd_case "suffix is 64-char lowercase hex" (fun () ->
            let name = gsd_cookie_of (Github_fixture.goc_fixture 'S') in
            let prefix_len = String.length Github_fixture.gsd_cookie_prefix in
            Alcotest.(check bool)
              "hex64" true
              (Github_fixture.goc_is_hex64
                 (String.sub name prefix_len (String.length name - prefix_len))));
        gsd_case "raw state is absent" (fun () ->
            Alcotest.(check bool)
              "no raw state" false
              (Html_assert.contains_nonempty
                 ~needle:(Github_fixture.goc_fixture 'S')
                 (gsd_cookie_of (Github_fixture.goc_fixture 'S'))));
        gsd_case "same state gives the same cookie name" (fun () ->
            Alcotest.(check string)
              "deterministic"
              (gsd_cookie_of (Github_fixture.goc_fixture 'S'))
              (gsd_cookie_of (Github_fixture.goc_fixture 'S')));
        gsd_case "different states give different cookie names" (fun () ->
            Alcotest.(check bool)
              "names differ" false
              (String.equal
                 (gsd_cookie_of (Github_fixture.goc_fixture 'S'))
                 (gsd_cookie_of (Github_fixture.goc_fixture 'T'))));
        gsd_case "no binding or verifier material" (fun () ->
            let name = gsd_cookie_of (Github_fixture.goc_fixture 'S') in
            Alcotest.(check bool)
              "no binding fixture" false
              (Html_assert.contains_nonempty ~needle:gsd_fixture_binding name);
            Alcotest.(check bool)
              "no verifier fixture" false
              (Html_assert.contains_nonempty ~needle:gsd_fixture_verifier name);
            let data = GSD.create () in
            Alcotest.(check bool)
              "no created verifier" false
              (Html_assert.contains_nonempty
                 ~needle:(Github_fixture.gsd_verifier data)
                 name);
            Alcotest.(check bool)
              "no created binding hash" false
              (Html_assert.contains_nonempty
                 ~needle:(Github_fixture.gsd_binding_hash data)
                 name));
        gsd_case "no whitespace or control characters" (fun () ->
            Alcotest.(check bool)
              "printable, no spaces" true
              (String.for_all
                 (fun c -> c > ' ' && c < '\x7f')
                 (gsd_cookie_of (Github_fixture.goc_fixture 'S'))));
      ] )
    (* Two concurrent flows: independent per-flow cookie names, each value
       round-trips under its own cookie, and encoding the second flow
       leaves the first flow's serialized value byte-identical. *);
    ( "github_session_data_parallel_flows",
      [
        gsd_case "flows keep distinct cookie names and values" (fun () ->
            let first = GSD.create () and second = GSD.create () in
            let first_encoded = GSD.encode first in
            let second_encoded = GSD.encode second in
            Alcotest.(check bool)
              "cookie names differ" false
              (String.equal
                 (gsd_cookie_of (Github_fixture.goc_fixture 'S'))
                 (gsd_cookie_of (Github_fixture.goc_fixture 'T')));
            let first_decoded = gsd_decode_exn "first" first_encoded in
            let second_decoded = gsd_decode_exn "second" second_encoded in
            Alcotest.(check bool)
              "first verifier survives" true
              (String.equal
                 (Github_fixture.gsd_verifier first)
                 (Github_fixture.gsd_verifier first_decoded));
            Alcotest.(check bool)
              "second verifier survives" true
              (String.equal
                 (Github_fixture.gsd_verifier second)
                 (Github_fixture.gsd_verifier second_decoded));
            Alcotest.(check bool)
              "first value untouched by second" true
              (String.equal first_encoded (GSD.encode first)));
      ] );
  ]
