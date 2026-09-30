module GUI = Earde.Github_user_installations

(* === GitHub user-installations verification (Github_user_installations) ===
   DB-free, network-free coverage through fake injected GET transports.
   Token-fixture assertions are boolean, so no token bytes reach test output
   on failure. The module's error type is payload-free except for the HTTP
   status integer, so error assertions may render errors as strings. *)

let gui_case = Case.quick

let gui_access_fixture = "gui-access.TOKEN~1"

(* The requested installation ID under test. *)
let gui_target = 424242L

(* A valid abstract token_set produced through the real exchange against a
   fake token-exchange transport — no test-only constructor exists. *)
let gui_token_set () =
  let captured = ref None in
  let outcome =
    Github_fixture.gte_exchange
      (Github_fixture.gte_transport
         (Ok
            ( 200,
              {|{"access_token":"gui-access.TOKEN~1","token_type":"bearer","scope":""}|}
            ))
         captured)
  in
  Github_fixture.gte_tokens_exn "user-installations token fixture" outcome

(* One full verification against the scripted responses; returns the outcome
   plus the captured requests in order. *)
let gui_verify ?(installation_id = 424242L) responses =
  let captured = ref [] in
  let outcome =
    Lwt_main.run
      (GUI.verify
         ~transport:(Github_fixture.gui_transport responses captured)
         ~token_set:(gui_token_set ()) ~installation_id)
  in
  (outcome, !captured)

let gui_expect_error label expected outcome =
  match outcome with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" label
  | Error actual ->
      Alcotest.(check string) label (Github_fixture.gui_show_error expected)
        (Github_fixture.gui_show_error actual)

let gui_raw_account ?(id = "424243") ?(login = {|"owner-424242"|}) () =
  Printf.sprintf {|{"id":%s,"login":%s}|} id login

let gui_show_account_type = function
  | GUI.User -> "User"
  | GUI.Organization -> "Organization"

let gui_invalid name body =
  gui_case name (fun () ->
      let outcome, requests = gui_verify [ Ok (200, body) ] in
      gui_expect_error name GUI.Invalid_response outcome;
      Alcotest.(check int) "stops after the malformed page" 1
        (List.length requests))

let suites =
    (* Installation verification, request construction: fixed endpoint,
       the exact two-parameter query in deterministic order, the exact
       four-header set, and the token only in the Authorization header. *)
  [ ( "github_user_installations_request"
    , [ gui_case "endpoint, query, and headers are exact" (fun () ->
            let _, requests = gui_verify [ Github_fixture.gui_page_of ~total:0 [] ] in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            Alcotest.(check (option string)) "scheme" (Some "https")
              (Uri.scheme uri);
            Alcotest.(check (option string)) "host" (Some "api.github.com")
              (Uri.host uri);
            Alcotest.(check string) "path" "/user/installations"
              (Uri.path uri);
            Alcotest.(check (option string)) "no fragment" None
              (Uri.fragment uri);
            Alcotest.(check (option string)) "no userinfo" None
              (Uri.userinfo uri);
            Alcotest.(check (list (pair string (list string))))
              "exactly per_page then page, deterministic order"
              [ ("per_page", [ "100" ]); ("page", [ "1" ]) ]
              (Uri.query uri);
            Alcotest.(check (list string))
              "exactly the four application headers, in order"
              [ "accept"; "authorization"; "x-github-api-version";
                "user-agent" ]
              (List.map fst headers);
            Alcotest.(check (option string)) "media type"
              (Some "application/vnd.github+json")
              (List.assoc_opt "accept" headers);
            Alcotest.(check (option string)) "API version"
              (Some "2026-03-10")
              (List.assoc_opt "x-github-api-version" headers);
            Alcotest.(check (option string)) "fixed User-Agent"
              (Some "Earde-GitHub-Onboarding")
              (List.assoc_opt "user-agent" headers);
            (* Boolean on purpose: the Authorization value must not reach
               test output on failure. *)
            Alcotest.(check bool)
              "Authorization is Bearer plus the token_set access token"
              true
              (match List.assoc_opt "authorization" headers with
              | Some value ->
                  String.equal value ("Bearer " ^ gui_access_fixture)
              | None -> false))
      ; gui_case "token and installation ID stay out of the URI" (fun () ->
            let _, requests = gui_verify [ Github_fixture.gui_page_of ~total:0 [] ] in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            let uri_string = Uri.to_string uri in
            Alcotest.(check bool) "access token absent from URI" false
              (Html_assert.contains_nonempty ~needle:gui_access_fixture uri_string);
            Alcotest.(check bool) "installation ID absent from URI" false
              (Html_assert.contains_nonempty ~needle:(Int64.to_string gui_target) uri_string);
            let header_text =
              String.concat "\n"
                (List.map (fun (k, v) -> k ^ ":" ^ v) headers)
            in
            Alcotest.(check bool) "installation ID absent from headers"
              false
              (Html_assert.contains_nonempty ~needle:(Int64.to_string gui_target)
                 header_text))
      ; gui_case "forbidden parameters and headers are absent" (fun () ->
            let _, requests = gui_verify [ Github_fixture.gui_page_of ~total:0 [] ] in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            let query_keys = List.map fst (Uri.query uri) in
            List.iter
              (fun key ->
                Alcotest.(check bool) (key ^ " absent from query") false
                  (List.mem key query_keys))
              [ "client_id"; "client_secret"; "code"; "state";
                "code_verifier"; "installation_id"; "access_token" ];
            let header_keys = List.map fst headers in
            List.iter
              (fun key ->
                Alcotest.(check bool) (key ^ " absent from headers") false
                  (List.mem key header_keys))
              [ "cookie"; "content-type" ])
      ] )
    (* First-page success: the target ID is found, exactly one request is
       made, and unknown fields at both levels are ignored. *)
  ; ( "github_user_installations_success"
    , [ gui_case "target on the first page" (fun () ->
            let outcome, requests =
              gui_verify
                [ Ok
                    ( 200,
                      {|{"total_count":2,"github_future_field":[1,2],"installations":[{"id":7,"account":{"id":70,"login":"someone"},"target_type":"User"},{"id":424242,"account":{"id":9099,"login":"earde-owner"},"target_type":"Organization","app_id":9,"repository_selection":"all","html_url":"https://github.com/x"}]}|}
                    )
                ]
            in
            let verified = Github_fixture.gui_verified_exn "first page" outcome in
            Alcotest.(check int64) "accessor returns the requested ID"
              gui_target
              (GUI.installation_id verified);
            Alcotest.(check int) "only one request" 1
              (List.length requests))
      ; gui_case "ID beyond OCaml int range verifies through int64"
          (fun () ->
            (* 2^62 does not fit a 63-bit OCaml int, so Yojson yields an
               `Intlit`; the match must still work exactly. *)
            let big = 4611686018427387904L in
            let outcome, requests =
              gui_verify ~installation_id:big
                [ Github_fixture.gui_page_of ~total:1 [ big ] ]
            in
            let verified = Github_fixture.gui_verified_exn "big id" outcome in
            Alcotest.(check int64) "exact int64 round-trip" big
              (GUI.installation_id verified);
            Alcotest.(check int) "only one request" 1
              (List.length requests))
      ] )
    (* Account identity: the verified result carries exactly the matching
       entry's account ID, login, and target type, with target_type — not
       account.type — deciding the variant. *)
  ; ( "github_user_installations_account"
    , [ gui_case "organization identity is preserved" (fun () ->
            let outcome, _ =
              gui_verify
                [ Ok
                    ( 200,
                      Github_fixture.gui_entry_page
                        {|{"id":424242,"account":{"id":9999,"login":"Café-Owner_1","avatar_url":"https://example.invalid/a.png"},"target_type":"Organization"}|}
                    )
                ]
            in
            let verified = Github_fixture.gui_verified_exn "organization" outcome in
            Alcotest.(check int64) "installation ID" gui_target
              (GUI.installation_id verified);
            Alcotest.(check int64) "account ID as int64" 9999L
              (GUI.account_id verified);
            (* Mixed case, punctuation, and a multibyte UTF-8 sequence:
               accepted logins come back byte-for-byte. *)
            Alcotest.(check string) "login byte-for-byte" "Café-Owner_1"
              (GUI.account_login verified);
            Alcotest.(check string) "target type" "Organization"
              (gui_show_account_type (GUI.account_type verified)))
      ; gui_case "user identity is preserved" (fun () ->
            let outcome, _ =
              gui_verify
                [ Ok
                    ( 200,
                      Github_fixture.gui_entry_page
                        (Github_fixture.gui_raw_entry ~target:{|"User"|} ()) )
                ]
            in
            let verified = Github_fixture.gui_verified_exn "user" outcome in
            Alcotest.(check int64) "installation ID" gui_target
              (GUI.installation_id verified);
            Alcotest.(check int64) "account ID as int64" 424243L
              (GUI.account_id verified);
            Alcotest.(check string) "login byte-for-byte" "owner-424242"
              (GUI.account_login verified);
            Alcotest.(check string) "target type" "User"
              (gui_show_account_type (GUI.account_type verified)))
      ; gui_case "account ID beyond OCaml int range round-trips" (fun () ->
            (* 2^62 does not fit a 63-bit OCaml int, so Yojson yields an
               `Intlit` for the account ID; the exact value must
               survive. *)
            let outcome, _ =
              gui_verify
                [ Ok
                    ( 200,
                      Github_fixture.gui_entry_page
                        (Github_fixture.gui_raw_entry
                           ~account:
                             (gui_raw_account ~id:"4611686018427387904" ())
                           ()) )
                ]
            in
            let verified = Github_fixture.gui_verified_exn "big account id" outcome in
            Alcotest.(check int64) "exact int64 round-trip"
              4611686018427387904L
              (GUI.account_id verified))
      ; gui_case "target_type is authoritative over account.type"
          (fun () ->
            let outcome, _ =
              gui_verify
                [ Ok
                    ( 200,
                      Github_fixture.gui_entry_page
                        {|{"id":424242,"account":{"id":9999,"login":"earde-owner","type":"User"},"target_type":"Organization"}|}
                    )
                ]
            in
            let verified = Github_fixture.gui_verified_exn "authority" outcome in
            Alcotest.(check string) "account.type ignored" "Organization"
              (gui_show_account_type (GUI.account_type verified)))
      ] )
    (* Definitive absence: a complete search that ends without the target
       reports Installation_not_accessible. *)
  ; ( "github_user_installations_absence"
    , [ gui_case "empty installations array" (fun () ->
            let outcome, requests = gui_verify [ Github_fixture.gui_page_of ~total:0 [] ] in
            gui_expect_error "empty" GUI.Installation_not_accessible
              outcome;
            Alcotest.(check int) "one request" 1 (List.length requests))
      ; gui_case "short page without the target" (fun () ->
            let outcome, requests =
              gui_verify [ Github_fixture.gui_page_of ~total:3 (Github_fixture.gui_ids ~from:1000L 3) ]
            in
            gui_expect_error "short page" GUI.Installation_not_accessible
              outcome;
            Alcotest.(check int) "one request" 1 (List.length requests))
      ; gui_case "full final page where page * 100 >= total_count"
          (fun () ->
            let outcome, requests =
              gui_verify
                [ Github_fixture.gui_page_of ~total:200 (Github_fixture.gui_ids ~from:1000L 100)
                ; Github_fixture.gui_page_of ~total:200 (Github_fixture.gui_ids ~from:2000L 100)
                ]
            in
            gui_expect_error "full final page"
              GUI.Installation_not_accessible outcome;
            Alcotest.(check (list string)) "both pages requested"
              [ "1"; "2" ]
              (List.map (Github_fixture.gui_requested_page "full final page") requests))
      ] )
    (* Pagination: sequential pages 1..n, stop on match, hard stop at
       page 5 reported as Pagination_limit — never as absence. *)
  ; ( "github_user_installations_pagination"
    , [ gui_case "target found on page 3, no page 4 request" (fun () ->
            let outcome, requests =
              gui_verify
                [ Github_fixture.gui_page_of ~total:250 (Github_fixture.gui_ids ~from:1000L 100)
                ; Github_fixture.gui_page_of ~total:250 (Github_fixture.gui_ids ~from:2000L 100)
                ; Github_fixture.gui_page_of ~total:250
                    (Github_fixture.gui_ids ~from:3000L 49 @ [ gui_target ])
                ]
            in
            let verified = Github_fixture.gui_verified_exn "page 3" outcome in
            Alcotest.(check int64) "target ID" gui_target
              (GUI.installation_id verified);
            Alcotest.(check (list string)) "pages 1, 2, 3 in order"
              [ "1"; "2"; "3" ]
              (List.map (Github_fixture.gui_requested_page "pagination") requests);
            List.iter
              (fun (uri, _) ->
                Alcotest.(check (option string)) "per_page on every page"
                  (Some "100")
                  (Uri.get_query_param uri "per_page"))
              requests)
      ; gui_case "five full pages hit the limit, no sixth request"
          (fun () ->
            let page n = Github_fixture.gui_page_of ~total:501 (Github_fixture.gui_ids ~from:(Int64.of_int (n * 1000)) 100) in
            let outcome, requests =
              gui_verify [ page 1; page 2; page 3; page 4; page 5 ]
            in
            gui_expect_error "limit" GUI.Pagination_limit outcome;
            Alcotest.(check (list string)) "pages 1 through 5"
              [ "1"; "2"; "3"; "4"; "5" ]
              (List.map (Github_fixture.gui_requested_page "limit") requests))
      ] )
    (* Invalid requested IDs are rejected before any request. *)
  ; ( "github_user_installations_input"
    , [ gui_case "zero ID makes no request" (fun () ->
            let outcome, requests = gui_verify ~installation_id:0L [] in
            gui_expect_error "zero" GUI.Invalid_installation_id outcome;
            Alcotest.(check int) "transport never invoked" 0
              (List.length requests))
      ; gui_case "negative ID makes no request" (fun () ->
            let outcome, requests = gui_verify ~installation_id:(-5L) [] in
            gui_expect_error "negative" GUI.Invalid_installation_id outcome;
            Alcotest.(check int) "transport never invoked" 0
              (List.length requests))
      ] )
    (* Invalid 200 bodies: anything that is not exactly a well-formed
       installations page maps to the payload-free Invalid_response, and
       a malformed entry poisons the whole page even after a match. *)
  ; ( "github_user_installations_invalid"
    , [ gui_invalid "invalid JSON" "not json at all"
      ; gui_invalid "top-level array"
          (Printf.sprintf {|[%s]|} (Github_fixture.gui_entry gui_target))
      ; gui_invalid "top-level scalar" "42"
      ; gui_invalid "top-level null" "null"
      ; gui_invalid "missing total_count"
          (Printf.sprintf {|{"installations":[%s]}|} (Github_fixture.gui_entry gui_target))
      ; gui_invalid "missing installations" {|{"total_count":1}|}
      ; gui_invalid "duplicate total_count"
          (Printf.sprintf
             {|{"total_count":1,"total_count":1,"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "duplicate installations"
          (Printf.sprintf
             {|{"total_count":1,"installations":[%s],"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target) (Github_fixture.gui_entry gui_target))
      ; gui_invalid "negative total_count"
          {|{"total_count":-1,"installations":[]}|}
      ; gui_invalid "float total_count"
          (Printf.sprintf {|{"total_count":1.0,"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "string total_count"
          (Printf.sprintf {|{"total_count":"1","installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "null total_count"
          (Printf.sprintf {|{"total_count":null,"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "boolean total_count"
          (Printf.sprintf {|{"total_count":true,"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "overflowing total_count"
          (Printf.sprintf
             {|{"total_count":9223372036854775808,"installations":[%s]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "installations not an array"
          (Printf.sprintf {|{"total_count":1,"installations":%s}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_case "more than 100 entries" (fun () ->
            let outcome, _ =
              gui_verify
                [ Github_fixture.gui_page_of ~total:101 (Github_fixture.gui_ids ~from:1000L 101) ]
            in
            gui_expect_error "oversized page" GUI.Invalid_response outcome)
      ; gui_invalid "entry not an object"
          {|{"total_count":1,"installations":[42]}|}
      ; gui_invalid "entry missing id"
          (Github_fixture.gui_entry_page
             {|{"account":{"id":424243,"login":"owner-424242"},"target_type":"Organization"}|})
      ; gui_invalid "duplicate id field in one entry"
          (Github_fixture.gui_entry_page
             {|{"id":424242,"id":424242,"account":{"id":424243,"login":"owner-424242"},"target_type":"Organization"}|})
      ; gui_invalid "zero id" (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~id:"0" ()))
      ; gui_invalid "negative id"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~id:"-7" ()))
      ; gui_invalid "float id"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~id:"424242.0" ()))
      ; gui_invalid "numeric-string id"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~id:{|"424242"|} ()))
      ; gui_invalid "overflowing id"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~id:"9223372036854775808" ()))
      ; gui_invalid "duplicate id across entries"
          (Printf.sprintf {|{"total_count":2,"installations":[%s,%s]}|}
             (Github_fixture.gui_entry 7L) (Github_fixture.gui_entry 7L))
      ; gui_invalid "malformed unrelated entry after a matching entry"
          (Printf.sprintf
             {|{"total_count":2,"installations":[%s,{"id":"bad"}]}|}
             (Github_fixture.gui_entry gui_target))
      ; gui_invalid "invalid account metadata after a matching entry"
          (Printf.sprintf
             {|{"total_count":2,"installations":[%s,{"id":7,"account":{"id":0,"login":"owner-7"},"target_type":"User"}]}|}
             (Github_fixture.gui_entry gui_target))
      ] )
    (* Invalid account metadata: each case takes an otherwise fully valid
       matching entry and breaks exactly one account or target_type
       aspect; all of them must poison the page as Invalid_response. *)
  ; ( "github_user_installations_account_invalid"
    , [ gui_invalid "missing account"
          (Github_fixture.gui_entry_page {|{"id":424242,"target_type":"Organization"}|})
      ; gui_invalid "duplicate account"
          (Github_fixture.gui_entry_page
             (Printf.sprintf
                {|{"id":424242,"account":%s,"account":%s,"target_type":"Organization"}|}
                (gui_raw_account ()) (gui_raw_account ())))
      ; gui_invalid "account not an object"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~account:"42" ()))
      ; gui_invalid "account is an array"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(Printf.sprintf "[%s]" (gui_raw_account ())) ()))
      ; gui_invalid "missing account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:{|{"login":"owner-424242"}|} ()))
      ; gui_invalid "duplicate account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:
                  {|{"id":424243,"id":424243,"login":"owner-424242"}|}
                ()))
      ; gui_invalid "zero account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~id:"0" ()) ()))
      ; gui_invalid "negative account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~id:"-9" ()) ()))
      ; gui_invalid "float account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~id:"424243.0" ()) ()))
      ; gui_invalid "numeric-string account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~id:{|"424243"|} ()) ()))
      ; gui_invalid "overflowing account id"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~id:"9223372036854775808" ()) ()))
      ; gui_invalid "missing login"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~account:{|{"id":424243}|} ()))
      ; gui_invalid "duplicate login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:{|{"id":424243,"login":"a","login":"a"}|} ()))
      ; gui_invalid "blank login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~login:{|""|} ()) ()))
      ; gui_invalid "whitespace-only login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~login:{|" "|} ()) ()))
      ; gui_invalid "leading whitespace in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|" owner"|} ()) ()))
      ; gui_invalid "trailing whitespace in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"owner "|} ()) ()))
      ; gui_invalid "internal whitespace in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own er"|} ()) ()))
      ; gui_invalid "tab in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own\ter"|} ()) ()))
      ; gui_invalid "newline in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own\ner"|} ()) ()))
      ; gui_invalid "control byte in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own\u0001er"|} ()) ()))
      ; gui_invalid "NUL in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own\u0000er"|} ()) ()))
      ; gui_invalid "DEL in login"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry
                ~account:(gui_raw_account ~login:{|"own\u007fer"|} ()) ()))
      ; gui_invalid "login of the wrong JSON type"
          (Github_fixture.gui_entry_page
             (Github_fixture.gui_raw_entry ~account:(gui_raw_account ~login:"42" ()) ()))
      ; gui_invalid "missing target_type"
          (Github_fixture.gui_entry_page
             (Printf.sprintf {|{"id":424242,"account":%s}|}
                (gui_raw_account ())))
      ; gui_invalid "duplicate target_type"
          (Github_fixture.gui_entry_page
             (Printf.sprintf
                {|{"id":424242,"account":%s,"target_type":"Organization","target_type":"Organization"}|}
                (gui_raw_account ())))
      ; gui_invalid "non-string target_type"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~target:"1" ()))
      ; gui_invalid "unknown target type"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~target:{|"Enterprise"|} ()))
      ; gui_invalid "lowercase user"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~target:{|"user"|} ()))
      ; gui_invalid "lowercase organization"
          (Github_fixture.gui_entry_page (Github_fixture.gui_raw_entry ~target:{|"organization"|} ()))
      ] )
    (* Transport and status failures: constructors carry at most the
       status integer, the remote body is never parsed or preserved, and
       pagination halts immediately. *)
  ; ( "github_user_installations_transport"
    , [ gui_case "transport failure maps to Transport_error" (fun () ->
            let outcome, requests = gui_verify [ Error () ] in
            gui_expect_error "transport" GUI.Transport_error outcome;
            Alcotest.(check int) "one request" 1 (List.length requests))
      ; gui_case "every non-200 preserves only the status integer"
          (fun () ->
            List.iter
              (fun status ->
                let outcome, requests =
                  gui_verify [ Ok (status, "irrelevant") ]
                in
                gui_expect_error
                  ("status " ^ string_of_int status)
                  (GUI.Unexpected_http_status status) outcome;
                Alcotest.(check int) "one request" 1
                  (List.length requests))
              [ 201; 302; 304; 401; 403; 500; 502 ])
      ; gui_case "valid-looking body on a non-200 is not parsed" (fun () ->
            let outcome, _ =
              gui_verify [ Ok (401, Github_fixture.gui_body ~total:1 [ gui_target ]) ]
            in
            gui_expect_error "401 with target in body"
              (GUI.Unexpected_http_status 401) outcome)
      ; gui_case "secret-looking response body cannot escape" (fun () ->
            let outcome, _ =
              gui_verify
                [ Ok (502, {|{"secret":"SHOULD-NOT-ESCAPE"}|}) ]
            in
            match outcome with
            | Error e ->
                Alcotest.(check bool) "no body detail in the rendering"
                  false
                  (Html_assert.contains_nonempty ~needle:"SHOULD-NOT-ESCAPE"
                     (Github_fixture.gui_show_error e))
            | Ok _ -> Alcotest.fail "expected Error, got Ok")
      ; gui_case "pagination stops on a mid-search transport failure"
          (fun () ->
            let outcome, requests =
              gui_verify
                [ Github_fixture.gui_page_of ~total:300 (Github_fixture.gui_ids ~from:1000L 100)
                ; Error ()
                ]
            in
            gui_expect_error "mid-search transport" GUI.Transport_error
              outcome;
            Alcotest.(check int) "exactly two requests" 2
              (List.length requests))
      ; gui_case "pagination stops on a mid-search status error" (fun () ->
            let outcome, requests =
              gui_verify
                [ Github_fixture.gui_page_of ~total:300 (Github_fixture.gui_ids ~from:1000L 100)
                ; Ok (503, "unavailable")
                ]
            in
            gui_expect_error "mid-search status"
              (GUI.Unexpected_http_status 503) outcome;
            Alcotest.(check int) "exactly two requests" 2
              (List.length requests))
      ] )
    (* Token privacy, checked structurally: the token appears in exactly
       one place — the Authorization header — and no returned value or
       rendered error can carry it. *)
  ; ( "github_user_installations_privacy"
    , [ gui_case "token appears only in the Authorization header"
          (fun () ->
            let _, requests = gui_verify [ Github_fixture.gui_page_of ~total:0 [] ] in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            Alcotest.(check bool) "absent from URI" false
              (Html_assert.contains_nonempty ~needle:gui_access_fixture (Uri.to_string uri));
            List.iter
              (fun (key, value) ->
                if not (String.equal key "authorization") then (
                  Alcotest.(check bool) (key ^ " name is token-free") false
                    (Html_assert.contains_nonempty ~needle:gui_access_fixture key);
                  Alcotest.(check bool) (key ^ " value is token-free")
                    false
                    (Html_assert.contains_nonempty ~needle:gui_access_fixture value)))
              headers)
      ; gui_case "rendered errors never contain the token" (fun () ->
            List.iter
              (fun responses ->
                let outcome, _ = gui_verify responses in
                match outcome with
                | Error e ->
                    Alcotest.(check bool) "token-free rendering" false
                      (Html_assert.contains_nonempty ~needle:gui_access_fixture
                         (Github_fixture.gui_show_error e))
                | Ok _ -> Alcotest.fail "expected Error, got Ok")
              [ [ Error () ]
              ; [ Ok (401, "denied") ]
              ; [ Ok (200, "not json at all") ]
              ; [ Github_fixture.gui_page_of ~total:0 [] ]
              ])
      ; gui_case "different bodies collapse to the same error variant"
          (fun () ->
            let render responses =
              match gui_verify responses with
              | Error e, _ -> Github_fixture.gui_show_error e
              | Ok _, _ -> Alcotest.fail "expected Error, got Ok"
            in
            Alcotest.(check string) "invalid bodies indistinguishable"
              (render [ Ok (200, "not json at all") ])
              (render
                 [ Ok (200, {|{"total_count":"x","installations":[]}|}) ]);
            Alcotest.(check string) "non-200 bodies indistinguishable"
              (render [ Ok (403, "rate limited detail") ])
              (render [ Ok (403, {|{"message":"forbidden"}|}) ]))
      ] )
  ]
