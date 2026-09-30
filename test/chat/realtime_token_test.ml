module RT = Earde.Realtime_token

(* Realtime token: pure signing/claims tests, no DB and no network. The signing
   secret comes from the environment, so each case pins it explicitly. The
   endpoint handler reuses create_for_topic verbatim, so topic binding, expiry
   and signature format proven here hold for freshly refreshed tokens too. *)

let with_secret secret f =
  Unix.putenv RT.secret_env secret;
  Fun.protect ~finally:(fun () -> Unix.putenv RT.secret_env "") f

let mint ?(shared_cursors = false) () =
  match
    RT.create_for_topic ~user_id:7 ~username:"alice" ~topic:"chan:8"
      ~shared_cursors
  with
  | Some token -> token
  | None -> Alcotest.fail "expected a token when the secret is set"

let claims_field token name =
  match RT.decode_payload token with
  | Some (`Assoc fields) -> List.assoc_opt name fields
  | _ -> Alcotest.fail "token payload did not decode to a JSON object"

(* Community feature gating (shared cursors): pure allow-list parsing and
   membership only — the env-reading wrapper is a trivial composition. *)
let check_slugs name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string)) name expected (Earde.Features.slugs_of_string raw))

let check_enabled name expected slugs community_slug =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Earde.Features.enabled_in ~slugs ~community_slug))

let rt_case name f = Alcotest.test_case name `Quick f

(* The shared_cursors claim: derived from the community capability at mint
   time. The refresh endpoint calls the same create_for_topic with a freshly
   recomputed Features value, so these cover refreshed tokens too. *)
let rt_capability_claim =
  rt_case "shared_cursors claim mirrors the community capability" (fun () ->
      with_secret "test-secret" (fun () ->
          let claim_for community_slug =
            let shared_cursors =
              Earde.Features.enabled_in ~slugs:[ "beryl" ] ~community_slug
            in
            let token =
              match
                RT.create_for_topic ~user_id:7 ~username:"alice"
                  ~topic:"chan:8" ~shared_cursors
              with
              | Some token -> token
              | None -> Alcotest.fail "expected a token"
            in
            claims_field token "shared_cursors"
          in
          Alcotest.(check bool) "enabled community mints true" true
            (claim_for "beryl" = Some (`Bool true));
          Alcotest.(check bool) "ordinary community mints false" true
            (claim_for "earde" = Some (`Bool false))))

let rt_capability_recomputed =
  rt_case "each mint recomputes the capability it is given" (fun () ->
      with_secret "test-secret" (fun () ->
          (* Same identity/topic, different capability inputs — as when a
             refresh happens after a community's capability changed. *)
          Alcotest.(check bool) "true stays true" true
            (claims_field (mint ~shared_cursors:true ()) "shared_cursors"
            = Some (`Bool true));
          Alcotest.(check bool) "false stays false" true
            (claims_field (mint ~shared_cursors:false ()) "shared_cursors"
            = Some (`Bool false))))

let rt_no_secret =
  rt_case "no secret => no token" (fun () ->
      Unix.putenv RT.secret_env "";
      Alcotest.(check bool) "None without secret" true
        (RT.create_for_topic ~user_id:7 ~username:"alice" ~topic:"chan:8"
           ~shared_cursors:false
        = None))

let rt_format =
  rt_case "token is base64url payload dot signature" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              let is_b64url s =
                s <> ""
                && String.for_all
                     (function
                       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
                       | _ -> false)
                     s
              in
              Alcotest.(check bool) "payload base64url" true (is_b64url payload);
              Alcotest.(check bool) "signature base64url" true (is_b64url signature)
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_topic_binding =
  rt_case "claims carry the exact topic, identity and version" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          Alcotest.(check bool) "topic" true
            (claims_field token "topic" = Some (`String "chan:8"));
          Alcotest.(check bool) "user_id" true
            (claims_field token "user_id" = Some (`Int 7));
          Alcotest.(check bool) "username" true
            (claims_field token "username" = Some (`String "alice"));
          Alcotest.(check bool) "version" true
            (claims_field token "v" = Some (`Int 1))))

let rt_expiry =
  rt_case "expiry is now + default ttl" (fun () ->
      with_secret "test-secret" (fun () ->
          let before = RT.unix_now () in
          let token = mint () in
          let after = RT.unix_now () in
          match claims_field token "exp" with
          | Some (`Int exp) ->
              Alcotest.(check bool) "exp lower bound" true
                (exp >= before + RT.default_ttl_seconds);
              Alcotest.(check bool) "exp upper bound" true
                (exp <= after + RT.default_ttl_seconds)
          | _ -> Alcotest.fail "exp claim missing or not an int"))

let rt_signature =
  rt_case "signature is HMAC-SHA256 of the payload part" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              Alcotest.(check string) "recomputed signature matches"
                (RT.hmac_sha256_base64url ~secret:"test-secret" payload)
                signature
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_tamper =
  rt_case "tampered payload no longer matches the signature" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              let tampered = payload ^ "A" in
              Alcotest.(check bool) "signature differs" true
                (RT.hmac_sha256_base64url ~secret:"test-secret" tampered
                <> signature)
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_decode_garbage =
  rt_case "decode_payload rejects garbage" (fun () ->
      Alcotest.(check bool) "no dot" true (RT.decode_payload "nodot" = None);
      Alcotest.(check bool) "two dots" true (RT.decode_payload "a.b.c" = None);
      Alcotest.(check bool) "non-json payload" true
        (RT.decode_payload "!!!.sig" = None))

let suites =
  [ ( "realtime_token"
    , [ rt_no_secret
      ; rt_format
      ; rt_topic_binding
      ; rt_expiry
      ; rt_signature
      ; rt_tamper
      ; rt_decode_garbage
      ; rt_capability_claim
      ; rt_capability_recomputed
      ] )
  ; ( "features_shared_cursors"
    , [ check_slugs "single" [ "beryl" ] "beryl"
      ; check_slugs "trims and lowercases" [ "beryl"; "earde" ] " Beryl , EARDE "
      ; check_slugs "drops empties" [ "a"; "b" ] ",a,,b,"
      ; check_slugs "empty string" [] ""
      ; check_enabled "member enabled" true [ "beryl" ] "beryl"
      ; check_enabled "non-member disabled" false [ "beryl" ] "earde"
      ; check_enabled "case-insensitive community" true [ "beryl" ] "Beryl"
      ; check_enabled "empty list disables" false [] "beryl"
      ; check_enabled "default list has beryl" true
          Earde.Features.default_shared_cursor_slugs "beryl"
      ; check_enabled "default list is only beryl" false
          Earde.Features.default_shared_cursor_slugs "earde"
      ] )
  ]
