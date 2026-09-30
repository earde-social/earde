module GOC = Earde.Github_onboarding_crypto
module GSD = Earde.Github_onboarding_session_data
module GCK = Earde.Github_onboarding_cookie

(* === GitHub onboarding cookie adapter (Github_onboarding_cookie) ===
   Exercises real Dream request/response cookie behavior under a fixed test
   secret installed through Dream's normal [set_secret] middleware, so
   encrypted cookies round-trip deterministically across simulated
   requests. Set-Cookie assertions are structural; material comparisons are
   boolean so no secret, plaintext, or ciphertext ever reaches test
   output. *)

let gck_case = Case.quick

let gck_http_config () =
  Github_fixture.gac_ok_exn "localhost cookie config"
    (Github_fixture.gac_of_values
       ~origin:(Some "http://localhost:8080")
       ~setup:
         (Some "http://localhost:8080/integrations/github/install/return")
       ~callback:
         (Some "http://localhost:8080/integrations/github/authorize/callback")
       ())

(* Fixed states so cookie names are deterministic; A and B model two
   concurrent onboarding flows in one browser. *)
let gck_state_a () = Github_fixture.goc_state_exn (Github_fixture.goc_fixture 'S')

let gck_state_b () = Github_fixture.goc_state_exn (Github_fixture.goc_fixture 'T')

let gck_drop_headers config state =
  Github_fixture.gck_with_request (fun request ->
      let response = Dream.response "" in
      GCK.drop config ~request ~response ~state;
      Dream.headers response "Set-Cookie")

let gck_load config state cookies =
  Github_fixture.gck_with_request ~cookies (fun request -> GCK.load config ~request ~state)

let gck_loaded label config state cookies =
  match gck_load config state cookies with
  | Ok data -> data
  | Error GCK.Missing -> Alcotest.failf "%s: unexpected Missing" label
  | Error GCK.Invalid -> Alcotest.failf "%s: unexpected Invalid" label

let gck_load_error label expected config state cookies =
  match gck_load config state cookies with
  | Ok _ -> Alcotest.failf "%s: expected an error, got Ok" label
  | Error actual ->
      let show = function
        | GCK.Missing -> "Missing"
        | GCK.Invalid -> "Invalid"
      in
      Alcotest.(check string) label (show expected) (show actual)

(* Shared policy assertions for one parsed Set-Cookie. [secure] toggles the
   production-versus-loopback expectations. *)
let gck_check_policy label ~secure (name, value, attributes) state data =
  let expected_name =
    (if secure then "__Secure-" else "") ^ GSD.cookie_name state
  in
  Alcotest.(check string) (label ^ ": browser-visible name") expected_name
    name;
  Alcotest.(check bool) (label ^ ": no __Host- prefix") false
    (Html_assert.contains_nonempty ~needle:"__Host-" name);
  Alcotest.(check bool) (label ^ ": Secure") secure
    (List.mem_assoc "secure" attributes);
  Alcotest.(check bool) (label ^ ": HttpOnly") true
    (List.mem_assoc "httponly" attributes);
  Alcotest.(check (option string)) (label ^ ": SameSite") (Some "lax")
    (List.assoc_opt "samesite" attributes);
  Alcotest.(check (option string))
    (label ^ ": Path")
    (Some "/integrations/github")
    (List.assoc_opt "path" attributes);
  (match List.assoc_opt "max-age" attributes with
  | None -> Alcotest.fail (label ^ ": Max-Age missing")
  | Some seconds ->
      Alcotest.(check bool) (label ^ ": Max-Age is 900") true
        (float_of_string seconds = 900.));
  Alcotest.(check bool) (label ^ ": no Domain") false
    (List.mem_assoc "domain" attributes);
  Alcotest.(check bool) (label ^ ": no Expires") false
    (List.mem_assoc "expires" attributes);
  Alcotest.(check bool) (label ^ ": value is encrypted") false
    (Html_assert.contains_nonempty ~needle:(GSD.encode data) value);
  Alcotest.(check bool) (label ^ ": raw state absent from name") false
    (Html_assert.contains_nonempty ~needle:(GOC.state_to_string state) name)

let suites =
    (* Cookie adapter, production https policy: __Secure- prefix, Secure,
       HttpOnly, SameSite=Lax, narrow path, 900-second Max-Age, host-only,
       encrypted value, and no raw state in the name. *)
  [ ( "github_onboarding_cookie_https_policy"
    , [ gck_case "store emits the full production policy" (fun () ->
            let state = gck_state_a () in
            let data = GSD.create () in
            let cookie =
              Github_fixture.gck_stored "https store" (Github_fixture.gck_https_config ()) state data
            in
            let name, _, _ = cookie in
            Alcotest.(check bool) "expected name prefix" true
              (String.starts_with
                 ~prefix:"__Secure-earde.github_onboarding.v1." name);
            gck_check_policy "https store" ~secure:true cookie state data)
      ] )
    (* Cookie adapter, loopback http development policy: same restricted
       attributes but no Secure and no name prefix. *)
  ; ( "github_onboarding_cookie_http_policy"
    , [ gck_case "store emits the loopback development policy" (fun () ->
            let state = gck_state_a () in
            let data = GSD.create () in
            let cookie =
              Github_fixture.gck_stored "http store" (gck_http_config ()) state data
            in
            let name, _, _ = cookie in
            Alcotest.(check bool) "no __Secure- prefix" false
              (Html_assert.contains_nonempty ~needle:"__Secure-" name);
            gck_check_policy "http store" ~secure:false cookie state data)
      ] )
    (* Store-then-load through a simulated browser transfer: the decoded
       material matches the original and decoding stays strict. *)
  ; ( "github_onboarding_cookie_roundtrip"
    , [ gck_case "https round trip preserves the material" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state = gck_state_a () in
            let data = GSD.create () in
            let name, value, _ =
              Github_fixture.gck_stored "https roundtrip" config state data
            in
            let loaded =
              gck_loaded "https roundtrip" config state [ (name, value) ]
            in
            Alcotest.(check bool) "same verifier" true
              (String.equal (Github_fixture.gsd_verifier data) (Github_fixture.gsd_verifier loaded));
            Alcotest.(check bool) "same binding hash" true
              (String.equal (Github_fixture.gsd_binding_hash data)
                 (Github_fixture.gsd_binding_hash loaded));
            Alcotest.(check bool) "same derived challenge" true
              (String.equal (Github_fixture.gsd_challenge data) (Github_fixture.gsd_challenge loaded));
            Alcotest.(check bool) "re-encodes byte-identically" true
              (String.equal (GSD.encode data) (GSD.encode loaded)))
      ; gck_case "http round trip preserves the material" (fun () ->
            let config = gck_http_config () in
            let state = gck_state_a () in
            let data = GSD.create () in
            let name, value, _ =
              Github_fixture.gck_stored "http roundtrip" config state data
            in
            let loaded =
              gck_loaded "http roundtrip" config state [ (name, value) ]
            in
            Alcotest.(check bool) "same verifier" true
              (String.equal (Github_fixture.gsd_verifier data) (Github_fixture.gsd_verifier loaded)))
      ] )
    (* Missing versus invalid: absence, undecryptable ciphertext under the
       expected raw name, validly encrypted but malformed plaintext, and a
       cookie stored for a different state. *)
  ; ( "github_onboarding_cookie_missing_invalid"
    , [ gck_case "no cookie is Missing" (fun () ->
            gck_load_error "absent cookie" GCK.Missing (Github_fixture.gck_https_config ())
              (gck_state_a ()) [])
      ; gck_case "corrupted ciphertext is Invalid" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state = gck_state_a () in
            let name, value, _ =
              Github_fixture.gck_stored "corrupt" config state (GSD.create ())
            in
            gck_load_error "corrupted ciphertext" GCK.Invalid config state
              [ (name, "AAAA" ^ value) ])
      ; gck_case "validly encrypted malformed plaintext is Invalid"
          (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state = gck_state_a () in
            (* Forged through Dream's encrypted-cookie primitive under the
               same policy, so decryption succeeds and only the strict
               plaintext decoder can reject it. *)
            let name, value, _ =
              Github_fixture.gck_single_set_cookie "forged plaintext"
                (Github_fixture.gck_with_request (fun request ->
                     let response = Dream.response "" in
                     Dream.set_cookie ~prefix:(Some `Secure) ~encrypt:true
                       ~max_age:900. ~path:(Some "/integrations/github")
                       ~secure:true ~http_only:true ~same_site:(Some `Lax)
                       response request
                       (GSD.cookie_name state)
                       "not-a-session-encoding";
                     Dream.headers response "Set-Cookie"))
            in
            gck_load_error "malformed plaintext" GCK.Invalid config state
              [ (name, value) ])
      ; gck_case "state A's cookie is Missing for state B" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let name, value, _ =
              Github_fixture.gck_stored "cross-state" config (gck_state_a ())
                (GSD.create ())
            in
            gck_load_error "other state" GCK.Missing config
              (gck_state_b ()) [ (name, value) ])
      ] )
    (* Deletion: same browser-visible identity and attribute policy as
       store, an expiry in the past, and idempotence. *)
  ; ( "github_onboarding_cookie_drop"
    , [ gck_case "https drop expires the stored cookie" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state = gck_state_a () in
            let stored_name, _, _ =
              Github_fixture.gck_stored "drop target" config state (GSD.create ())
            in
            let name, _, attributes =
              Github_fixture.gck_single_set_cookie "https drop"
                (gck_drop_headers config state)
            in
            Alcotest.(check string) "same browser-visible name" stored_name
              name;
            Alcotest.(check (option string)) "same path"
              (Some "/integrations/github")
              (List.assoc_opt "path" attributes);
            Alcotest.(check bool) "Secure" true
              (List.mem_assoc "secure" attributes);
            Alcotest.(check bool) "HttpOnly" true
              (List.mem_assoc "httponly" attributes);
            Alcotest.(check bool) "expired" true
              (match List.assoc_opt "expires" attributes with
              | Some date -> Html_assert.contains_nonempty ~needle:"1970" date
              | None ->
                  List.assoc_opt "max-age" attributes = Some "0"))
      ; gck_case "http drop uses the development identity" (fun () ->
            let config = gck_http_config () in
            let state = gck_state_a () in
            let stored_name, _, _ =
              Github_fixture.gck_stored "http drop target" config state (GSD.create ())
            in
            let name, _, attributes =
              Github_fixture.gck_single_set_cookie "http drop"
                (gck_drop_headers config state)
            in
            Alcotest.(check string) "same browser-visible name" stored_name
              name;
            Alcotest.(check bool) "no __Secure- prefix" false
              (Html_assert.contains_nonempty ~needle:"__Secure-" name);
            Alcotest.(check bool) "no Secure attribute" false
              (List.mem_assoc "secure" attributes))
      ; gck_case "drop is idempotent" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state = gck_state_a () in
            Alcotest.(check (list string)) "identical deletion headers"
              (gck_drop_headers config state)
              (gck_drop_headers config state))
      ] )
    (* Two concurrent flows in one browser: independent names, independent
       loads, and dropping one leaves the other intact. *)
  ; ( "github_onboarding_cookie_parallel_flows"
    , [ gck_case "flows store, load, and drop independently" (fun () ->
            let config = Github_fixture.gck_https_config () in
            let state_a = gck_state_a () and state_b = gck_state_b () in
            let data_a = GSD.create () and data_b = GSD.create () in
            (* Both stores on one response: distinct headers, nothing
               overwritten. *)
            let headers =
              Github_fixture.gck_with_request (fun request ->
                  let response = Dream.response "" in
                  GCK.store config ~request ~response ~state:state_a data_a;
                  GCK.store config ~request ~response ~state:state_b data_b;
                  Dream.headers response "Set-Cookie")
            in
            let name_a, value_a, _ =
              match headers with
              | [ first; _ ] -> Github_fixture.gck_parse_set_cookie first
              | _ -> Alcotest.fail "expected two Set-Cookie headers"
            in
            let name_b, value_b, _ =
              match headers with
              | [ _; second ] -> Github_fixture.gck_parse_set_cookie second
              | _ -> Alcotest.fail "expected two Set-Cookie headers"
            in
            Alcotest.(check bool) "distinct names" false
              (String.equal name_a name_b);
            let jar = [ (name_a, value_a); (name_b, value_b) ] in
            let loaded_a = gck_loaded "flow A" config state_a jar in
            let loaded_b = gck_loaded "flow B" config state_b jar in
            Alcotest.(check bool) "A keeps its verifier" true
              (String.equal (Github_fixture.gsd_verifier data_a) (Github_fixture.gsd_verifier loaded_a));
            Alcotest.(check bool) "B keeps its verifier" true
              (String.equal (Github_fixture.gsd_verifier data_b) (Github_fixture.gsd_verifier loaded_b));
            let drop_name, _, _ =
              Github_fixture.gck_single_set_cookie "drop A"
                (gck_drop_headers config state_a)
            in
            Alcotest.(check string) "drop targets A" name_a drop_name;
            Alcotest.(check bool) "drop does not target B" false
              (String.equal drop_name name_b);
            (* Apply A's deletion in the simulated browser. *)
            let jar =
              List.filter
                (fun (name, _) -> not (String.equal name drop_name))
                jar
            in
            gck_load_error "A gone after deletion" GCK.Missing config
              state_a jar;
            let survivor = gck_loaded "B survives" config state_b jar in
            Alcotest.(check bool) "B still round-trips" true
              (String.equal (Github_fixture.gsd_verifier data_b) (Github_fixture.gsd_verifier survivor)))
      ] )
  ]
