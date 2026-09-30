module GPK = Earde.Github_onboarding_pkce
module GAC = Earde.Github_app_config
module GTE = Earde.Github_oauth_token_exchange

(* === GitHub OAuth token exchange (Github_oauth_token_exchange) ===
   DB-free, network-free coverage through fake injected TRANSPORT modules.
   Assertions on codes, secrets, verifiers, and tokens are boolean, so no
   credential fixture bytes reach test output on failure. The module's error
   type is payload-free except for the HTTP status integer, so error
   assertions may render errors as strings. *)

let gte_case = Case.quick

let gte_code_ok name raw =
  gte_case name (fun () ->
      match GTE.authorization_code_of_callback raw with
      | Ok _ -> ()
      | Error GTE.Invalid_code -> Alcotest.fail "expected Ok, got Error")

let gte_code_err name raw =
  gte_case name (fun () ->
      match GTE.authorization_code_of_callback raw with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GTE.Invalid_code -> ())

(* One full exchange against a fake transport answering [result]; returns
   the outcome plus the captured (uri, headers, body). *)
let gte_run_result result =
  let captured = ref None in
  let outcome =
    Github_fixture.gte_exchange (Github_fixture.gte_transport result captured)
  in
  match !captured with
  | Some request -> (outcome, request)
  | None -> Alcotest.fail "transport was never called"

let gte_run ?(status = 200) ~response () =
  gte_run_result (Ok (status, response))

let gte_expect_error label expected outcome =
  match outcome with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" label
  | Error actual ->
      Alcotest.(check string)
        label
        (Github_fixture.gte_show_error expected)
        (Github_fixture.gte_show_error actual)

let gte_invalid name response =
  gte_case name (fun () ->
      let outcome, _ = gte_run ~response () in
      gte_expect_error name GTE.Invalid_response outcome)

let gte_rejected name response =
  gte_case name (fun () ->
      let outcome, _ = gte_run ~response () in
      gte_expect_error name GTE.OAuth_rejected outcome)

(* Decoded form entries of a captured request body, order preserved. *)
let gte_form body = Uri.query_of_encoded body

let gte_form_single label key form =
  match List.filter (fun (k, _) -> String.equal k key) form with
  | [ (_, [ value ]) ] -> value
  | _ -> Alcotest.failf "%s: expected exactly one %s value" label key

let gte_access_fixture = "gte-access.TOKEN~1"
let gte_refresh_fixture = "gte-refresh.TOKEN~2"

(* An expiring-configuration success body with substitutable raw JSON for
   the three expiration-related fields, so each duration/refresh rejection
   case varies exactly one field. *)
let gte_expiring_body ?(expires = "28800")
    ?(refresh = {|"gte-refresh.TOKEN~2"|}) ?(refresh_expires = "15811200") () =
  Printf.sprintf
    {|{"access_token":"gte-access.TOKEN~1","token_type":"bearer","scope":"","expires_in":%s,"refresh_token":%s,"refresh_token_expires_in":%s}|}
    expires refresh refresh_expires

let suites =
  (* Authorization-code intake: opaque and byte-exact — no format is
       imposed, but whitespace/control damage is rejected, never
       repaired. *)
  [
    ( "github_token_exchange_code",
      [
        gte_code_ok "representative opaque code" "a1b2c3d4e5f6a1b2c3d4";
        gte_code_ok "punctuation accepted" Github_fixture.gte_code_string;
        gte_code_ok "single byte, no fixed length" "x";
        gte_code_ok "long code, no fixed length" (String.make 300 'k');
        gte_code_ok "digits only, no fixed alphabet" "1234567890";
        gte_code_ok "no fixed prefix required" "not-a-gho-prefix";
        gte_code_err "empty" "";
        gte_code_err "spaces only" "   ";
        gte_code_err "leading whitespace" " abc";
        gte_code_err "trailing whitespace" "abc ";
        gte_code_err "internal space" "ab cd";
        gte_code_err "tab" "ab\tcd";
        gte_code_err "LF" "ab\ncd";
        gte_code_err "CR" "ab\rcd";
        gte_code_err "NUL" "ab\x00cd";
        gte_code_err "control byte" "ab\x01cd";
        gte_code_err "DEL" "ab\x7fcd";
        gte_case "accepted bytes reach the form byte-for-byte" (fun () ->
            let _, (_, _, body) = gte_run ~response:"{}" () in
            Alcotest.(check bool)
              "code round-trips form encoding" true
              (String.equal Github_fixture.gte_code_string
                 (gte_form_single "captured form" "code" (gte_form body))));
      ] )
    (* Request construction, checked structurally on a captured request:
       fixed endpoint, exactly two application headers, the exact five-key
       form in deterministic order, and no credential outside the body. *);
    ( "github_token_exchange_request",
      [
        gte_case "endpoint, headers, and form are exact" (fun () ->
            let _, (uri, headers, body) = gte_run ~response:"{}" () in
            Alcotest.(check (option string))
              "scheme" (Some "https") (Uri.scheme uri);
            Alcotest.(check (option string))
              "host" (Some "github.com") (Uri.host uri);
            Alcotest.(check string)
              "path" "/login/oauth/access_token" (Uri.path uri);
            Alcotest.(check (list (pair string (list string))))
              "no query" [] (Uri.query uri);
            Alcotest.(check (option string))
              "no fragment" None (Uri.fragment uri);
            Alcotest.(check (option string))
              "no userinfo" None (Uri.userinfo uri);
            Alcotest.(check (list (pair string string)))
              "exactly the two application headers"
              [
                ("accept", "application/json");
                ("content-type", "application/x-www-form-urlencoded");
              ]
              headers;
            let form = gte_form body in
            Alcotest.(check (list string))
              "deterministic five-key order"
              [
                "client_id";
                "client_secret";
                "code";
                "redirect_uri";
                "code_verifier";
              ]
              (List.map fst form);
            let config = Github_fixture.gte_config () in
            Alcotest.(check bool)
              "client_id from config" true
              (String.equal (GAC.client_id config)
                 (gte_form_single "form" "client_id" form));
            Alcotest.(check bool)
              "client_secret from credentials" true
              (String.equal Github_fixture.gte_client_secret
                 (gte_form_single "form" "client_secret" form));
            Alcotest.(check bool)
              "redirect_uri is the validated callback" true
              (String.equal (GAC.callback_url config)
                 (gte_form_single "form" "redirect_uri" form));
            Alcotest.(check bool)
              "original verifier, not the challenge" true
              (String.equal Github_fixture.gte_verifier_string
                 (gte_form_single "form" "code_verifier" form));
            Alcotest.(check bool)
              "challenge absent from body" false
              (Html_assert.contains_nonempty
                 ~needle:
                   (GPK.challenge_to_string
                      (GPK.challenge_of_verifier
                         (Github_fixture.gte_verifier ())))
                 body));
        gte_case "credentials ride only in the body" (fun () ->
            let _, (uri, headers, _) = gte_run ~response:"{}" () in
            let uri_string = Uri.to_string uri in
            let header_text =
              String.concat "\n" (List.map (fun (k, v) -> k ^ ":" ^ v) headers)
            in
            List.iter
              (fun (label, needle) ->
                Alcotest.(check bool)
                  (label ^ " absent from URI")
                  false
                  (Html_assert.contains_nonempty ~needle uri_string);
                Alcotest.(check bool)
                  (label ^ " absent from headers")
                  false
                  (Html_assert.contains_nonempty ~needle header_text))
              [
                ("client secret", Github_fixture.gte_client_secret);
                ("code", Github_fixture.gte_code_string);
                ("verifier", Github_fixture.gte_verifier_string);
              ]);
        gte_case "forbidden parameters are absent" (fun () ->
            let _, (_, _, body) = gte_run ~response:"{}" () in
            let keys = List.map fst (gte_form body) in
            List.iter
              (fun key ->
                Alcotest.(check bool)
                  (key ^ " absent") false (List.mem key keys))
              [
                "scope";
                "grant_type";
                "repository_id";
                "installation_id";
                "state";
                "code_challenge";
                "code_challenge_method";
              ]);
      ] )
    (* Successful 200 parsing: bearer + empty scope required; the three
       expiration-related fields travel all-or-nothing. *);
    ( "github_token_exchange_success",
      [
        gte_case "non-expiring configuration" (fun () ->
            let outcome, _ =
              gte_run
                ~response:
                  {|{"access_token":"gte-access.TOKEN~1","token_type":"bearer","scope":"","github_future_field":[1,2]}|}
                ()
            in
            let tokens = Github_fixture.gte_tokens_exn "non-expiring" outcome in
            Alcotest.(check bool)
              "access token exact" true
              (String.equal gte_access_fixture (GTE.access_token tokens));
            Alcotest.(check bool)
              "no expires_in" true
              (GTE.expires_in tokens = None);
            Alcotest.(check bool)
              "no refresh token" true
              (GTE.refresh_token tokens = None);
            Alcotest.(check bool)
              "no refresh expiry" true
              (GTE.refresh_token_expires_in tokens = None));
        gte_case "expiring configuration" (fun () ->
            let outcome, _ = gte_run ~response:(gte_expiring_body ()) () in
            let tokens = Github_fixture.gte_tokens_exn "expiring" outcome in
            Alcotest.(check bool)
              "access token exact" true
              (String.equal gte_access_fixture (GTE.access_token tokens));
            Alcotest.(check bool)
              "refresh token exact" true
              (match GTE.refresh_token tokens with
              | Some refresh -> String.equal gte_refresh_fixture refresh
              | None -> false);
            Alcotest.(check (option int))
              "expires_in" (Some 28800) (GTE.expires_in tokens);
            Alcotest.(check (option int))
              "refresh expiry" (Some 15811200)
              (GTE.refresh_token_expires_in tokens));
        gte_case "durations are parsed, not hard-coded" (fun () ->
            let outcome, _ =
              gte_run
                ~response:
                  (gte_expiring_body ~expires:"1" ~refresh_expires:"1" ())
                ()
            in
            let tokens =
              Github_fixture.gte_tokens_exn "one-second durations" outcome
            in
            Alcotest.(check (option int))
              "expires_in" (Some 1) (GTE.expires_in tokens);
            Alcotest.(check (option int))
              "refresh expiry" (Some 1)
              (GTE.refresh_token_expires_in tokens));
      ] )
    (* 200 OAuth rejections: every well-formed remote error collapses to
       the same payload-free constructor; contradictory or malformed error
       shapes are invalid, so the client is not a remote-error oracle. *);
    ( "github_token_exchange_rejection",
      [
        gte_rejected "well-formed rejection"
          {|{"error":"bad_verification_code","error_description":"The code passed is incorrect or expired.","error_uri":"https://docs.github.com/x"}|};
        gte_rejected "different remote error, same constructor"
          {|{"error":"incorrect_client_credentials"}|};
        gte_case "remote error text cannot escape" (fun () ->
            let outcome, _ =
              gte_run
                ~response:
                  {|{"error":"redirect_uri_mismatch","error_description":"SECRET-LOOKING-DETAIL","error_uri":"https://evil.example/oracle"}|}
                ()
            in
            gte_expect_error "collapsed" GTE.OAuth_rejected outcome;
            match outcome with
            | Error e ->
                Alcotest.(check bool)
                  "no detail in the rendering" false
                  (Html_assert.contains_nonempty ~needle:"SECRET-LOOKING-DETAIL"
                     (Github_fixture.gte_show_error e))
            | Ok _ -> Alcotest.fail "expected Error, got Ok");
        gte_invalid "error mixed with access token"
          {|{"error":"bad_verification_code","access_token":"t","token_type":"bearer","scope":""}|};
        gte_invalid "error mixed with refresh token"
          {|{"error":"bad_verification_code","refresh_token":"r"}|};
        gte_invalid "blank error" {|{"error":""}|};
        gte_invalid "whitespace-only error" {|{"error":"   "}|};
        gte_invalid "wrong-type error" {|{"error":42}|};
        gte_invalid "duplicate error" {|{"error":"a","error":"b"}|};
      ] )
    (* Invalid 200 bodies: anything that is not exactly a well-formed
       success or rejection object maps to the payload-free
       Invalid_response. *);
    ( "github_token_exchange_invalid",
      [
        gte_invalid "invalid JSON" "not json at all";
        gte_invalid "top-level array" {|[{"access_token":"t"}]|};
        gte_invalid "top-level string" {|"gho_token"|};
        gte_invalid "top-level null" "null";
        gte_invalid "missing access token"
          {|{"token_type":"bearer","scope":""}|};
        gte_invalid "missing token type" {|{"access_token":"t","scope":""}|};
        gte_invalid "wrong-case token type"
          {|{"access_token":"t","token_type":"Bearer","scope":""}|};
        gte_invalid "non-bearer token type"
          {|{"access_token":"t","token_type":"mac","scope":""}|};
        gte_invalid "missing scope"
          {|{"access_token":"t","token_type":"bearer"}|};
        gte_invalid "non-empty scope"
          {|{"access_token":"t","token_type":"bearer","scope":"repo"}|};
        gte_invalid "empty access token"
          {|{"access_token":"","token_type":"bearer","scope":""}|};
        gte_invalid "access token with space"
          {|{"access_token":"t t","token_type":"bearer","scope":""}|};
        gte_invalid "access token with newline"
          {|{"access_token":"t\nt","token_type":"bearer","scope":""}|};
        gte_invalid "access token with NUL"
          {|{"access_token":"t\u0000t","token_type":"bearer","scope":""}|};
        gte_invalid "access token with DEL"
          {|{"access_token":"t\u007ft","token_type":"bearer","scope":""}|};
        gte_invalid "wrong-type access token"
          {|{"access_token":42,"token_type":"bearer","scope":""}|};
        gte_invalid "duplicate access token"
          {|{"access_token":"a","access_token":"b","token_type":"bearer","scope":""}|};
        gte_invalid "duplicate token type"
          {|{"access_token":"t","token_type":"bearer","token_type":"bearer","scope":""}|};
        gte_invalid "duplicate scope"
          {|{"access_token":"t","token_type":"bearer","scope":"","scope":""}|};
        gte_invalid "partial triplet: expires_in only"
          {|{"access_token":"t","token_type":"bearer","scope":"","expires_in":28800}|};
        gte_invalid "partial triplet: refresh token only"
          {|{"access_token":"t","token_type":"bearer","scope":"","refresh_token":"r"}|};
        gte_invalid "partial triplet: refresh expiry missing"
          {|{"access_token":"t","token_type":"bearer","scope":"","expires_in":28800,"refresh_token":"r"}|};
        gte_invalid "partial triplet: expires_in missing"
          {|{"access_token":"t","token_type":"bearer","scope":"","refresh_token":"r","refresh_token_expires_in":15811200}|};
        gte_invalid "refresh token with whitespace"
          (gte_expiring_body ~refresh:{|"bad token"|} ());
        gte_invalid "empty refresh token" (gte_expiring_body ~refresh:{|""|} ());
        gte_invalid "wrong-type refresh token"
          (gte_expiring_body ~refresh:"42" ());
        gte_invalid "zero expires_in" (gte_expiring_body ~expires:"0" ());
        gte_invalid "negative expires_in" (gte_expiring_body ~expires:"-1" ());
        gte_invalid "float expires_in" (gte_expiring_body ~expires:"3600.5" ());
        gte_invalid "string expires_in"
          (gte_expiring_body ~expires:{|"28800"|} ());
        gte_invalid "null expires_in" (gte_expiring_body ~expires:"null" ());
        gte_invalid "overflowing expires_in"
          (gte_expiring_body ~expires:"99999999999999999999999999" ());
        gte_invalid "zero refresh expiry"
          (gte_expiring_body ~refresh_expires:"0" ());
        gte_invalid "float refresh expiry"
          (gte_expiring_body ~refresh_expires:"1.5" ());
        gte_invalid "duplicate expires_in"
          {|{"access_token":"t","token_type":"bearer","scope":"","expires_in":1,"expires_in":1,"refresh_token":"r","refresh_token_expires_in":1}|};
      ] )
    (* Transport and status failures: transport errors and non-200
       statuses collapse to constructors carrying at most the status
       integer — the remote body is never parsed or preserved. *);
    ( "github_token_exchange_transport",
      [
        gte_case "transport failure maps to Transport_error" (fun () ->
            let outcome, _ = gte_run_result (Error ()) in
            gte_expect_error "transport" GTE.Transport_error outcome);
        gte_case "non-200 preserves only the status integer" (fun () ->
            let outcome, _ =
              gte_run ~status:502
                ~response:
                  {|{"access_token":"gte-leak.SHOULD-NOT-ESCAPE","token_type":"bearer","scope":""}|}
                ()
            in
            (match outcome with
            | Error (GTE.Unexpected_http_status 502) -> ()
            | Error e ->
                Alcotest.failf "wrong error: %s"
                  (Github_fixture.gte_show_error e)
            | Ok _ -> Alcotest.fail "expected Error, got Ok");
            match outcome with
            | Error e ->
                Alcotest.(check bool)
                  "secret-looking body cannot appear in the error" false
                  (Html_assert.contains_nonempty ~needle:"SHOULD-NOT-ESCAPE"
                     (Github_fixture.gte_show_error e))
            | Ok _ -> ());
        gte_case "201 with a valid body is still not success" (fun () ->
            let outcome, _ =
              gte_run ~status:201 ~response:(gte_expiring_body ()) ()
            in
            gte_expect_error "201" (GTE.Unexpected_http_status 201) outcome);
        gte_case "redirect status is not followed" (fun () ->
            let outcome, _ = gte_run ~status:302 ~response:"" () in
            gte_expect_error "302" (GTE.Unexpected_http_status 302) outcome);
        gte_case "server error keeps its status" (fun () ->
            let outcome, _ = gte_run ~status:404 ~response:"not found" () in
            gte_expect_error "404" (GTE.Unexpected_http_status 404) outcome);
      ] );
  ]
