module GUI = Earde.Github_user_installations
module GUR = Earde.Github_user_installation_repositories

(* === GitHub installation repositories (Github_user_installation_repositories) ===
   DB-free, network-free coverage through fake injected GET transports.
   Token-set fixtures go through the real exchange and verified-installation
   fixtures through the real verify — no test-only constructor exists.
   Assertions on the token and on private-repository fixture strings are
   boolean, so neither can reach test output on failure. The module's error
   type is payload-free except for the HTTP status integer, so error
   assertions may render errors as strings. *)

let gur_case = Case.quick
let gur_access_fixture = "gur-access.TOKEN~1"

(* The verified installation fixture whose repositories are listed. *)
let gur_installation_id = 424242L

(* A valid abstract token_set produced through the real exchange against a
   fake token-exchange transport. *)
let gur_token_set () =
  let captured = ref None in
  let outcome =
    Github_fixture.gte_exchange
      (Github_fixture.gte_transport
         (Ok
            ( 200,
              {|{"access_token":"gur-access.TOKEN~1","token_type":"bearer","scope":""}|}
            ))
         captured)
  in
  Github_fixture.gte_tokens_exn "repositories token fixture" outcome

(* An abstract verified installation produced through the real verify
   against a one-page scripted installations listing. *)
let gur_installation ?(installation_id = gur_installation_id)
    ?(account_id = Github_fixture.gur_account_id)
    ?(login = Github_fixture.gur_login) ?(target = "Organization") () =
  let body =
    Printf.sprintf
      {|{"total_count":1,"installations":[{"id":%Ld,"account":{"id":%Ld,"login":"%s"},"target_type":"%s"}]}|}
      installation_id account_id login target
  in
  let captured = ref [] in
  let outcome =
    Lwt_main.run
      (GUI.verify
         ~transport:(Github_fixture.gui_transport [ Ok (200, body) ] captured)
         ~token_set:(gur_token_set ()) ~installation_id)
  in
  Github_fixture.gui_verified_exn "repositories installation fixture" outcome

(* One full listing against the scripted responses; returns the outcome
   plus the captured requests in order. *)
let gur_list ?installation responses =
  let installation =
    match installation with
    | Some installation -> installation
    | None -> gur_installation ()
  in
  let captured = ref [] in
  let outcome =
    Lwt_main.run
      (GUR.list_public
         ~transport:(Github_fixture.gur_transport responses captured)
         ~token_set:(gur_token_set ()) ~installation)
  in
  (outcome, !captured)

let gur_expect_error label expected outcome =
  match outcome with
  | Ok _ -> Alcotest.failf "%s: expected Error, got Ok" label
  | Error actual ->
      Alcotest.(check string)
        label
        (Github_fixture.gur_show_error expected)
        (Github_fixture.gur_show_error actual)

let gur_set_exn label outcome =
  match outcome with
  | Ok set -> GUR.repositories set
  | Error e ->
      Alcotest.failf "%s: listing failed with %s" label
        (Github_fixture.gur_show_error e)

let gur_private_repo ?(name = Github_fixture.gur_private_name)
    ?(private_flag = true) ?(visibility = "private") ~id () =
  Github_fixture.gur_repo ~id ~name ~private_flag ~visibility
    ~description:
      (Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
    ()

(* Entry and owner bodies with substitutable raw JSON per field, so a
   rejection case changes one field and inherits valid defaults for the
   rest. *)
let gur_raw_repo ?(id = "1001") ?(name = {|"alpha"|})
    ?(full_name = {|"earde-owner/alpha"|})
    ?(owner = {|{"id":9099,"login":"earde-owner"}|}) ?(private_json = "false")
    ?(visibility = {|"public"|}) ?(description = "null")
    ?(default_branch = {|"main"|}) ?(archived = "false") () =
  Printf.sprintf
    {|{"id":%s,"name":%s,"full_name":%s,"owner":%s,"private":%s,"visibility":%s,"description":%s,"default_branch":%s,"archived":%s}|}
    id name full_name owner private_json visibility description default_branch
    archived

let gur_raw_owner ?(id = "9099") ?(login = {|"earde-owner"|}) () =
  Printf.sprintf {|{"id":%s,"login":%s}|} id login

(* The valid default entry as an association list, for the exhaustive
   missing-field and duplicate-field sweeps. *)
let gur_default_fields =
  [
    ("id", "1001");
    ("name", {|"alpha"|});
    ("full_name", {|"earde-owner/alpha"|});
    ("owner", {|{"id":9099,"login":"earde-owner"}|});
    ("private", "false");
    ("visibility", {|"public"|});
    ("description", "null");
    ("default_branch", {|"main"|});
    ("archived", "false");
  ]

let gur_recognized_keys = List.map fst gur_default_fields

let gur_repo_assoc fields =
  "{"
  ^ String.concat ","
      (List.map (fun (k, v) -> Printf.sprintf {|"%s":%s|} k v) fields)
  ^ "}"

let gur_repo_without key =
  gur_repo_assoc
    (List.filter (fun (k, _) -> not (String.equal k key)) gur_default_fields)

let gur_repo_duplicating key =
  gur_repo_assoc
    (gur_default_fields
    @ List.filter (fun (k, _) -> String.equal k key) gur_default_fields)

let gur_page ~total entries = Ok (200, Github_fixture.gur_body ~total entries)

(* A single-entry page whose entry JSON is supplied raw. *)
let gur_entry_page entry = Github_fixture.gur_body ~total:1 [ entry ]

(* [count] sequential public filler repositories starting at [from]. *)
let gur_fill ~from count =
  List.init count (fun i ->
      let id = Int64.add from (Int64.of_int i) in
      Github_fixture.gur_repo ~id ~name:(Printf.sprintf "repo-%Ld" id) ())

let gur_invalid name body =
  gur_case name (fun () ->
      let outcome, requests = gur_list [ Ok (200, body) ] in
      gur_expect_error name GUR.Invalid_response outcome;
      Alcotest.(check int)
        "stops after the malformed page" 1 (List.length requests))

(* Everything a returned repository can render, concatenated for boolean
   leak checks. *)
let gur_repo_strings repo =
  String.concat "|"
    [
      GUR.owner_login repo;
      GUR.name repo;
      GUR.full_name repo;
      GUR.html_url repo;
      GUR.default_branch repo;
      (match GUR.description repo with None -> "" | Some d -> d);
    ]

let gur_check_no_private_material repos =
  List.iter
    (fun repo ->
      let rendered = gur_repo_strings repo in
      Alcotest.(check bool)
        "private name absent from accessors" false
        (Html_assert.contains_nonempty ~needle:Github_fixture.gur_private_name
           rendered);
      Alcotest.(check bool)
        "private description absent from accessors" false
        (Html_assert.contains_nonempty
           ~needle:Github_fixture.gur_private_description rendered))
    repos

let suites =
  (* Installation repositories, request construction: fixed scheme and
       host, the verified installation ID in the path only, the exact
       two-parameter query in deterministic order, the exact four-header
       set, and the production transport shared with the installations
       client. *)
  [
    ( "github_user_installation_repositories_request",
      [
        gur_case "endpoint, query, and headers are exact" (fun () ->
            let _, requests =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            Alcotest.(check (option string))
              "scheme" (Some "https") (Uri.scheme uri);
            Alcotest.(check (option string))
              "host" (Some "api.github.com") (Uri.host uri);
            Alcotest.(check string)
              "path with the verified ID"
              "/user/installations/424242/repositories" (Uri.path uri);
            Alcotest.(check (option string))
              "no fragment" None (Uri.fragment uri);
            Alcotest.(check (option string))
              "no userinfo" None (Uri.userinfo uri);
            Alcotest.(check (list (pair string (list string))))
              "exactly per_page then page, deterministic order"
              [ ("per_page", [ "100" ]); ("page", [ "1" ]) ]
              (Uri.query uri);
            Alcotest.(check (list string))
              "exactly the four application headers, in order"
              [
                "accept"; "authorization"; "x-github-api-version"; "user-agent";
              ]
              (List.map fst headers);
            Alcotest.(check (option string))
              "media type" (Some "application/vnd.github+json")
              (List.assoc_opt "accept" headers);
            Alcotest.(check (option string))
              "API version" (Some "2026-03-10")
              (List.assoc_opt "x-github-api-version" headers);
            Alcotest.(check (option string))
              "fixed User-Agent" (Some "Earde-GitHub-Onboarding")
              (List.assoc_opt "user-agent" headers);
            (* Boolean on purpose: the Authorization value must not reach
               test output on failure. *)
            Alcotest.(check bool)
              "Authorization is Bearer plus the token_set access token" true
              (match List.assoc_opt "authorization" headers with
              | Some value -> String.equal value ("Bearer " ^ gur_access_fixture)
              | None -> false));
        gur_case "path ID comes from the verified installation" (fun () ->
            let installation = gur_installation ~installation_id:31337L () in
            let _, requests =
              gur_list ~installation
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            match requests with
            | [ (uri, _) ] ->
                Alcotest.(check string)
                  "path" "/user/installations/31337/repositories" (Uri.path uri)
            | _ -> Alcotest.fail "expected exactly one request");
        gur_case
          "token out of the URI, installation ID out of query and headers"
          (fun () ->
            let _, requests =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            Alcotest.(check bool)
              "access token absent from URI" false
              (Html_assert.contains_nonempty ~needle:gur_access_fixture
                 (Uri.to_string uri));
            List.iter
              (fun (_, values) ->
                List.iter
                  (fun value ->
                    Alcotest.(check bool)
                      "installation ID absent from query values" false
                      (Html_assert.contains_nonempty
                         ~needle:(Int64.to_string gur_installation_id)
                         value))
                  values)
              (Uri.query uri);
            let header_text =
              String.concat "\n" (List.map (fun (k, v) -> k ^ ":" ^ v) headers)
            in
            Alcotest.(check bool)
              "installation ID absent from headers" false
              (Html_assert.contains_nonempty
                 ~needle:(Int64.to_string gur_installation_id)
                 header_text));
        gur_case "forbidden parameters and headers are absent" (fun () ->
            let _, requests =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            let query_keys = List.map fst (Uri.query uri) in
            List.iter
              (fun key ->
                Alcotest.(check bool)
                  (key ^ " absent from query")
                  false (List.mem key query_keys))
              [
                "client_id";
                "client_secret";
                "code";
                "state";
                "code_verifier";
                "installation_id";
                "access_token";
              ];
            let header_keys = List.map fst headers in
            List.iter
              (fun key ->
                Alcotest.(check bool)
                  (key ^ " absent from headers")
                  false (List.mem key header_keys))
              [ "cookie"; "content-type" ]);
        gur_case "production transport is the installations transport"
          (fun () ->
            (* Physical equality: the alias must re-export the existing
               transport, not compile a third copy of it. *)
            Alcotest.(check bool)
              "same get function" true
              (GUR.Cohttp_transport.get == GUI.Cohttp_transport.get));
      ] )
    (* Public repository parsing: every accessor, canonical URL
       construction, UTF-8 preservation, int64-exact IDs, and unknown
       fields — including a response-supplied html_url — ignored. *);
    ( "github_user_installation_repositories_parsing",
      [
        gur_case "personal-account repository accessors" (fun () ->
            let installation =
              gur_installation ~account_id:777L ~login:"solo-dev" ~target:"User"
                ()
            in
            let entry =
              Github_fixture.gur_repo ~owner_id:777L ~owner_login:"solo-dev"
                ~description:{|"Attrezzi — café ☕ tools."|}
                ~default_branch:"trunk" ~id:555L ~name:"tools" ()
            in
            let outcome, _ =
              gur_list ~installation [ gur_page ~total:1 [ entry ] ]
            in
            let repo =
              match gur_set_exn "personal" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            Alcotest.(check int64) "repository ID" 555L (GUR.repository_id repo);
            Alcotest.(check int64) "owner ID" 777L (GUR.owner_id repo);
            Alcotest.(check string)
              "owner login" "solo-dev" (GUR.owner_login repo);
            Alcotest.(check string) "name" "tools" (GUR.name repo);
            Alcotest.(check string)
              "full name" "solo-dev/tools" (GUR.full_name repo);
            Alcotest.(check string)
              "canonical HTML URL" "https://github.com/solo-dev/tools"
              (GUR.html_url repo);
            Alcotest.(check (option string))
              "UTF-8 description byte-for-byte"
              (Some "Attrezzi — café ☕ tools.") (GUR.description repo);
            Alcotest.(check string)
              "default branch" "trunk" (GUR.default_branch repo);
            Alcotest.(check bool) "not archived" false (GUR.is_archived repo));
        gur_case "organization repository: null description, archived"
          (fun () ->
            let entry =
              Github_fixture.gur_repo ~description:"null" ~archived:true
                ~id:1001L ~name:"Alpha-Repo_1.x" ()
            in
            let outcome, _ = gur_list [ gur_page ~total:1 [ entry ] ] in
            let repo =
              match gur_set_exn "organization" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            Alcotest.(check int64)
              "owner ID is the account ID" Github_fixture.gur_account_id
              (GUR.owner_id repo);
            Alcotest.(check string)
              "owner login" Github_fixture.gur_login (GUR.owner_login repo);
            Alcotest.(check string)
              "case preserved" "Alpha-Repo_1.x" (GUR.name repo);
            Alcotest.(check (option string))
              "null description is None" None (GUR.description repo);
            Alcotest.(check bool)
              "archived public repository is retained and flagged" true
              (GUR.is_archived repo));
        gur_case "slash-separated default branches are preserved exactly"
          (fun () ->
            (* Branch names are opaque snapshots: slashes and mixed case
               must survive parsing, set construction, and the accessor
               with no normalization, trimming, or splitting. *)
            let branches =
              [
                "main";
                "release/v1";
                "feature/onboarding/github";
                "Version-2/Stable";
              ]
            in
            let entries =
              List.mapi
                (fun i branch ->
                  Github_fixture.gur_repo ~default_branch:branch
                    ~id:(Int64.of_int (1101 + i))
                    ~name:(Printf.sprintf "branchy-%d" i)
                    ())
                branches
            in
            let outcome, _ = gur_list [ gur_page ~total:4 entries ] in
            let repos = gur_set_exn "slash branches" outcome in
            Alcotest.(check (list string))
              "byte-for-byte, untrimmed, unsplit" branches
              (List.map GUR.default_branch repos));
        gur_case "IDs beyond OCaml int range round-trip through int64"
          (fun () ->
            (* 2^62 does not fit a 63-bit OCaml int, so Yojson yields
               `Intlit`s; the exact values must survive. *)
            let big_account = 4611686018427387905L in
            let big_repo = 4611686018427387904L in
            let installation =
              gur_installation ~account_id:big_account ~login:"big-owner" ()
            in
            let entry =
              Github_fixture.gur_repo ~owner_id:big_account
                ~owner_login:"big-owner" ~id:big_repo ~name:"big" ()
            in
            let outcome, _ =
              gur_list ~installation [ gur_page ~total:1 [ entry ] ]
            in
            let repo =
              match gur_set_exn "big ids" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            Alcotest.(check int64)
              "repository ID exact" big_repo (GUR.repository_id repo);
            Alcotest.(check int64)
              "owner ID exact" big_account (GUR.owner_id repo));
        gur_case "canonical URL is structurally clean" (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            let repo =
              match gur_set_exn "url" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            let url = Uri.of_string (GUR.html_url repo) in
            Alcotest.(check (option string))
              "scheme" (Some "https") (Uri.scheme url);
            Alcotest.(check (option string))
              "host" (Some "github.com") (Uri.host url);
            Alcotest.(check string) "path" "/earde-owner/alpha" (Uri.path url);
            Alcotest.(check (list (pair string (list string))))
              "no query" [] (Uri.query url);
            Alcotest.(check (option string))
              "no fragment" None (Uri.fragment url);
            Alcotest.(check (option string))
              "no userinfo" None (Uri.userinfo url));
        gur_case "unknown fields and response html_url are ignored" (fun () ->
            let entry =
              gur_repo_assoc
                (gur_default_fields
                @ [
                    ("html_url", {|"https://evil.example/steer"|});
                    ("stargazers_count", "42");
                    ("permissions", {|{"admin":true}|});
                  ])
            in
            let body =
              Printf.sprintf
                {|{"total_count":1,"github_future_field":[1,2],"repositories":[%s]}|}
                entry
            in
            let outcome, _ = gur_list [ Ok (200, body) ] in
            let repo =
              match gur_set_exn "unknown fields" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            Alcotest.(check string)
              "URL is constructed, not the response html_url"
              "https://github.com/earde-owner/alpha" (GUR.html_url repo);
            Alcotest.(check bool)
              "response URL never escapes" false
              (Html_assert.contains_nonempty ~needle:"evil.example"
                 (gur_repo_strings repo)));
      ] )
    (* Public-only filtering: non-public entries are validated, silently
       excluded from the set, and none of their metadata can surface. *);
    ( "github_user_installation_repositories_filtering",
      [
        gur_case "private repository is omitted" (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:2
                    [
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                      gur_private_repo ~id:1002L ();
                    ];
                ]
            in
            let repos = gur_set_exn "private omitted" outcome in
            Alcotest.(check (list string))
              "only the public repository" [ "alpha" ] (List.map GUR.name repos);
            gur_check_no_private_material repos);
        gur_case "internal repository is omitted" (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:2
                    [
                      gur_private_repo ~private_flag:false
                        ~visibility:"internal" ~id:1002L ();
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                    ];
                ]
            in
            let repos = gur_set_exn "internal omitted" outcome in
            Alcotest.(check (list string))
              "only the public repository" [ "alpha" ] (List.map GUR.name repos);
            gur_check_no_private_material repos);
        gur_case "private flag and visibility must both say public" (fun () ->
            (* Disagreeing signals are structurally valid but never
               eligible, in either direction. *)
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:3
                    [
                      gur_private_repo ~private_flag:true ~visibility:"public"
                        ~id:1002L ();
                      gur_private_repo ~name:"secret-repo-fixture-b"
                        ~private_flag:false ~visibility:"Public" ~id:1003L ();
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                    ];
                ]
            in
            let repos = gur_set_exn "both signals" outcome in
            Alcotest.(check (list string))
              "only the fully public entry" [ "alpha" ]
              (List.map GUR.name repos);
            gur_check_no_private_material repos);
        gur_case "mixed page preserves public order" (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:4
                    [
                      gur_private_repo ~id:2001L ();
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                      gur_private_repo ~name:"secret-repo-fixture-b" ~id:2002L
                        ();
                      Github_fixture.gur_repo ~id:1002L ~name:"beta" ();
                    ];
                ]
            in
            let repos = gur_set_exn "mixed page" outcome in
            Alcotest.(check (list string))
              "public order preserved" [ "alpha"; "beta" ]
              (List.map GUR.name repos);
            gur_check_no_private_material repos);
        gur_case "only non-public repositories is No_public_repositories"
          (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:2
                    [
                      gur_private_repo ~id:2001L ();
                      gur_private_repo ~name:"secret-repo-fixture-b"
                        ~private_flag:false ~visibility:"internal" ~id:2002L ();
                    ];
                ]
            in
            gur_expect_error "only private" GUR.No_public_repositories outcome;
            Alcotest.(check int) "one request" 1 (List.length requests));
        gur_case "empty listing is No_public_repositories" (fun () ->
            let outcome, requests = gur_list [ gur_page ~total:0 [] ] in
            gur_expect_error "empty" GUR.No_public_repositories outcome;
            Alcotest.(check int) "one request" 1 (List.length requests));
      ] )
    (* Ownership and consistency: every entry — public or not — must be
       owned by the verified installation's account ID, agree with its own
       full_name, and never repeat an ID or full name. *);
    ( "github_user_installation_repositories_ownership",
      [
        gur_case "owner login may differ from the stored installation login"
          (fun () ->
            (* Logins are renamable; the stable check is the account ID. *)
            let entry =
              Github_fixture.gur_repo ~owner_login:"renamed-owner" ~id:1001L
                ~name:"alpha" ()
            in
            let outcome, _ = gur_list [ gur_page ~total:1 [ entry ] ] in
            let repo =
              match gur_set_exn "renamed login" outcome with
              | [ repo ] -> repo
              | repos ->
                  Alcotest.failf "expected one repository, got %d"
                    (List.length repos)
            in
            Alcotest.(check string)
              "refreshed login is returned" "renamed-owner"
              (GUR.owner_login repo);
            Alcotest.(check string)
              "full name follows the refreshed login" "renamed-owner/alpha"
              (GUR.full_name repo));
        gur_invalid "owner ID differing from the account ID"
          (gur_entry_page
             (gur_raw_repo ~owner:(gur_raw_owner ~id:"8888" ()) ()));
        gur_case "owner-ID mismatch on a non-public entry poisons the page"
          (fun () ->
            let mismatched_private =
              gur_raw_repo ~id:"1002" ~name:{|"beta"|}
                ~full_name:{|"earde-owner/beta"|}
                ~owner:(gur_raw_owner ~id:"8888" ())
                ~private_json:"true" ~visibility:{|"private"|} ()
            in
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:2
                    [
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                      mismatched_private;
                    ];
                ]
            in
            gur_expect_error "private ownership mismatch" GUR.Invalid_response
              outcome);
        gur_invalid "owner not an object"
          (gur_entry_page (gur_raw_repo ~owner:"42" ()));
        gur_invalid "owner is an array"
          (gur_entry_page
             (gur_raw_repo ~owner:(Printf.sprintf "[%s]" (gur_raw_owner ())) ()));
        gur_invalid "missing owner id"
          (gur_entry_page (gur_raw_repo ~owner:{|{"login":"earde-owner"}|} ()));
        gur_invalid "duplicate owner id"
          (gur_entry_page
             (gur_raw_repo
                ~owner:{|{"id":9099,"id":9099,"login":"earde-owner"}|} ()));
        gur_invalid "missing owner login"
          (gur_entry_page (gur_raw_repo ~owner:{|{"id":9099}|} ()));
        gur_invalid "duplicate owner login"
          (gur_entry_page
             (gur_raw_repo
                ~owner:
                  {|{"id":9099,"login":"earde-owner","login":"earde-owner"}|}
                ()));
        gur_invalid "empty owner login"
          (gur_entry_page
             (gur_raw_repo ~owner:(gur_raw_owner ~login:{|""|} ()) ()));
        gur_invalid "whitespace in owner login"
          (gur_entry_page
             (gur_raw_repo
                ~owner:(gur_raw_owner ~login:{|"earde owner"|} ())
                ()));
        gur_invalid "slash in owner login"
          (gur_entry_page
             (gur_raw_repo
                ~owner:(gur_raw_owner ~login:{|"earde/owner"|} ())
                ()));
        gur_invalid "control byte in owner login"
          (gur_entry_page
             (gur_raw_repo
                ~owner:(gur_raw_owner ~login:{|"earde\u0001owner"|} ())
                ()));
        gur_invalid "DEL in owner login"
          (gur_entry_page
             (gur_raw_repo
                ~owner:(gur_raw_owner ~login:{|"earde\u007fowner"|} ())
                ()));
        gur_invalid "non-string owner login"
          (gur_entry_page
             (gur_raw_repo ~owner:(gur_raw_owner ~login:"42" ()) ()));
        gur_invalid "slash in repository name"
          (gur_entry_page
             (gur_raw_repo ~name:{|"al/pha"|}
                ~full_name:{|"earde-owner/al/pha"|} ()));
        gur_case "slash-tolerant branches do not license a slash name"
          (fun () ->
            (* A valid slash-branch entry beside a slash-name entry: the
               branch policy must not leak into path-segment fields. *)
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:2
                    [
                      Github_fixture.gur_repo ~default_branch:"release/v1"
                        ~id:1001L ~name:"alpha" ();
                      gur_raw_repo ~id:"1002" ~name:{|"be/ta"|}
                        ~full_name:{|"earde-owner/be/ta"|} ();
                    ];
                ]
            in
            gur_expect_error "slash name still rejected" GUR.Invalid_response
              outcome);
        gur_case "slash-tolerant branches do not license a slash login"
          (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:2
                    [
                      Github_fixture.gur_repo ~default_branch:"release/v1"
                        ~id:1001L ~name:"alpha" ();
                      gur_raw_repo ~id:"1002" ~name:{|"beta"|}
                        ~full_name:{|"earde/owner/beta"|}
                        ~owner:(gur_raw_owner ~login:{|"earde/owner"|} ())
                        ();
                    ];
                ]
            in
            gur_expect_error "slash login still rejected" GUR.Invalid_response
              outcome);
        gur_invalid "full_name not owner/name"
          (gur_entry_page (gur_raw_repo ~full_name:{|"earde-owner/beta"|} ()));
        gur_invalid "full_name with a foreign owner"
          (gur_entry_page (gur_raw_repo ~full_name:{|"someone/alpha"|} ()));
        gur_invalid "full_name missing the slash"
          (gur_entry_page (gur_raw_repo ~full_name:{|"earde-owneralpha"|} ()));
        gur_invalid "full_name differing only by case"
          (gur_entry_page (gur_raw_repo ~full_name:{|"Earde-owner/alpha"|} ()));
        gur_invalid "duplicate repository ID in one page"
          (Github_fixture.gur_body ~total:2
             [
               Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
               Github_fixture.gur_repo ~id:1001L ~name:"beta" ();
             ]);
        gur_invalid "duplicate full name in one page"
          (Github_fixture.gur_body ~total:2
             [
               Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
               Github_fixture.gur_repo ~id:1002L ~name:"alpha" ();
             ]);
        gur_case "duplicate repository ID across pages" (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:200 (gur_fill ~from:1000L 100);
                  gur_page ~total:200
                    (Github_fixture.gur_repo ~id:1000L ~name:"dup-id" ()
                    :: gur_fill ~from:3000L 99);
                ]
            in
            gur_expect_error "duplicate ID across pages" GUR.Invalid_response
              outcome;
            Alcotest.(check int)
              "stops after the duplicate page" 2 (List.length requests));
        gur_case "duplicate full name across pages" (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:200 (gur_fill ~from:1000L 100);
                  gur_page ~total:200
                    (Github_fixture.gur_repo ~id:5555L ~name:"repo-1000" ()
                    :: gur_fill ~from:3000L 99);
                ]
            in
            gur_expect_error "duplicate full name across pages"
              GUR.Invalid_response outcome;
            Alcotest.(check int)
              "stops after the duplicate page" 2 (List.length requests));
        gur_case "duplicated non-public repository poisons the response"
          (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:3
                    [
                      Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                      gur_private_repo ~id:2001L ();
                      gur_private_repo ~id:2001L ();
                    ];
                ]
            in
            gur_expect_error "duplicated private entry" GUR.Invalid_response
              outcome);
      ] )
    (* Invalid 200 bodies: anything that is not exactly a well-formed
       repositories page maps to the payload-free Invalid_response, and a
       malformed entry poisons the whole page — never a partial result. *);
    ( "github_user_installation_repositories_invalid",
      [
        gur_invalid "invalid JSON" "not json at all";
        gur_invalid "top-level array"
          (Printf.sprintf {|[%s]|} (gur_raw_repo ()));
        gur_invalid "top-level scalar" "42";
        gur_invalid "top-level null" "null";
        gur_invalid "missing total_count"
          (Printf.sprintf {|{"repositories":[%s]}|} (gur_raw_repo ()));
        gur_invalid "missing repositories" {|{"total_count":1}|};
        gur_invalid "duplicate total_count"
          (Printf.sprintf
             {|{"total_count":1,"total_count":1,"repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "duplicate repositories"
          (Printf.sprintf
             {|{"total_count":1,"repositories":[%s],"repositories":[%s]}|}
             (gur_raw_repo ()) (gur_raw_repo ()));
        gur_invalid "negative total_count"
          {|{"total_count":-1,"repositories":[]}|};
        gur_invalid "float total_count"
          (Printf.sprintf {|{"total_count":1.0,"repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "string total_count"
          (Printf.sprintf {|{"total_count":"1","repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "null total_count"
          (Printf.sprintf {|{"total_count":null,"repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "boolean total_count"
          (Printf.sprintf {|{"total_count":true,"repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "overflowing total_count"
          (Printf.sprintf
             {|{"total_count":9223372036854775808,"repositories":[%s]}|}
             (gur_raw_repo ()));
        gur_invalid "repositories not an array"
          (Printf.sprintf {|{"total_count":1,"repositories":%s}|}
             (gur_raw_repo ()));
        gur_case "more than 100 entries" (fun () ->
            let outcome, _ =
              gur_list [ gur_page ~total:101 (gur_fill ~from:1000L 101) ]
            in
            gur_expect_error "oversized page" GUR.Invalid_response outcome);
        gur_invalid "entry not an object"
          {|{"total_count":1,"repositories":[42]}|};
      ]
      (* Every recognized repository field is required exactly once. *)
      @ List.map
          (fun key ->
            gur_invalid ("missing " ^ key)
              (gur_entry_page (gur_repo_without key)))
          gur_recognized_keys
      @ List.map
          (fun key ->
            gur_invalid ("duplicate " ^ key)
              (gur_entry_page (gur_repo_duplicating key)))
          gur_recognized_keys
      @ [
          gur_invalid "zero id" (gur_entry_page (gur_raw_repo ~id:"0" ()));
          gur_invalid "negative id" (gur_entry_page (gur_raw_repo ~id:"-7" ()));
          gur_invalid "float id" (gur_entry_page (gur_raw_repo ~id:"1001.0" ()));
          gur_invalid "numeric-string id"
            (gur_entry_page (gur_raw_repo ~id:{|"1001"|} ()));
          gur_invalid "overflowing id"
            (gur_entry_page (gur_raw_repo ~id:"9223372036854775808" ()));
          gur_invalid "non-string name"
            (gur_entry_page (gur_raw_repo ~name:"42" ()));
          gur_invalid "empty name"
            (gur_entry_page
               (gur_raw_repo ~name:{|""|} ~full_name:{|"earde-owner/"|} ()));
          gur_invalid "whitespace in name"
            (gur_entry_page
               (gur_raw_repo ~name:{|"al pha"|}
                  ~full_name:{|"earde-owner/al pha"|} ()));
          gur_invalid "non-string full_name"
            (gur_entry_page (gur_raw_repo ~full_name:"42" ()));
          gur_invalid "string private flag"
            (gur_entry_page (gur_raw_repo ~private_json:{|"false"|} ()));
          gur_invalid "null private flag"
            (gur_entry_page (gur_raw_repo ~private_json:"null" ()));
          gur_invalid "numeric visibility"
            (gur_entry_page (gur_raw_repo ~visibility:"1" ()));
          gur_invalid "null visibility"
            (gur_entry_page (gur_raw_repo ~visibility:"null" ()));
          gur_invalid "numeric description"
            (gur_entry_page (gur_raw_repo ~description:"42" ()));
          gur_invalid "boolean description"
            (gur_entry_page (gur_raw_repo ~description:"true" ()));
          gur_invalid "NUL in description"
            (gur_entry_page (gur_raw_repo ~description:{|"be\u0000tween"|} ()));
          gur_invalid "control byte in description"
            (gur_entry_page (gur_raw_repo ~description:{|"be\u0001tween"|} ()));
          gur_invalid "newline in description"
            (gur_entry_page (gur_raw_repo ~description:{|"line\nbreak"|} ()));
          gur_invalid "non-string default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:"42" ()));
          gur_invalid "empty default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|""|} ()));
          gur_invalid "whitespace-only default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|" "|} ()));
          gur_invalid "internal space in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma in"|} ()));
          gur_invalid "leading whitespace in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|" main"|} ()));
          gur_invalid "trailing whitespace in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"main "|} ()));
          gur_invalid "tab in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\tin"|} ()));
          gur_invalid "newline in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\nin"|} ()));
          gur_invalid "carriage return in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\rin"|} ()));
          gur_invalid "NUL in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\u0000in"|} ()));
          gur_invalid "control byte in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\u0001in"|} ()));
          gur_invalid "DEL in default branch"
            (gur_entry_page (gur_raw_repo ~default_branch:{|"ma\u007fin"|} ()));
          gur_invalid "string archived flag"
            (gur_entry_page (gur_raw_repo ~archived:{|"false"|} ()));
          gur_invalid "malformed trailing entry after valid public entries"
            (Github_fixture.gur_body ~total:3
               [
                 Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                 Github_fixture.gur_repo ~id:1002L ~name:"beta" ();
                 {|{"id":"bad"}|};
               ]);
          gur_case "trailing entry with a slash default branch is accepted"
            (fun () ->
              (* Formerly the only rejected property of this entry; the
                 page must now succeed whole, not partially. *)
              let outcome, _ =
                gur_list
                  [
                    gur_page ~total:2
                      [
                        Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                        Github_fixture.gur_repo ~default_branch:"release/v1"
                          ~id:1002L ~name:"beta" ();
                      ];
                  ]
              in
              let repos = gur_set_exn "slash-branch trailing" outcome in
              Alcotest.(check (list string))
                "both retained" [ "alpha"; "beta" ] (List.map GUR.name repos);
              match repos with
              | [ _; beta ] ->
                  Alcotest.(check string)
                    "branch preserved" "release/v1" (GUR.default_branch beta)
              | _ -> Alcotest.fail "expected exactly two repositories");
          gur_invalid "malformed private entry after valid public entries"
            (Github_fixture.gur_body ~total:2
               [
                 Github_fixture.gur_repo ~id:1001L ~name:"alpha" ();
                 gur_raw_repo ~id:"2001" ~name:{|"gamma"|}
                   ~full_name:{|"earde-owner/gamma"|} ~private_json:"true"
                   ~visibility:{|"private"|} ~archived:{|"broken"|} ();
               ]);
        ] )
    (* Pagination: sequential pages 1..n, definitive ends, global order,
       and the hard 20-page stop reported as Pagination_limit — never as
       a complete result. *);
    ( "github_user_installation_repositories_pagination",
      [
        gur_case "short first page completes in one request" (fun () ->
            let outcome, requests =
              gur_list [ gur_page ~total:3 (gur_fill ~from:1000L 3) ]
            in
            let repos = gur_set_exn "short first page" outcome in
            Alcotest.(check int) "three repositories" 3 (List.length repos);
            Alcotest.(check int) "one request" 1 (List.length requests));
        gur_case "full page covering total_count completes" (fun () ->
            let outcome, requests =
              gur_list [ gur_page ~total:100 (gur_fill ~from:1000L 100) ]
            in
            let repos = gur_set_exn "covered total" outcome in
            Alcotest.(check int) "all hundred returned" 100 (List.length repos);
            Alcotest.(check int) "no second request" 1 (List.length requests));
        gur_case "sequential multi-page success preserves global order"
          (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:250 (gur_fill ~from:1000L 100);
                  gur_page ~total:250 (gur_fill ~from:2000L 100);
                  gur_page ~total:250 (gur_fill ~from:3000L 50);
                ]
            in
            let repos = gur_set_exn "multi-page" outcome in
            let expected_ids =
              List.concat_map
                (fun from ->
                  Github_fixture.gui_ids ~from
                    (if Int64.equal from 3000L then 50 else 100))
                [ 1000L; 2000L; 3000L ]
            in
            Alcotest.(check (list int64))
              "GitHub order across pages" expected_ids
              (List.map GUR.repository_id repos);
            Alcotest.(check (list string))
              "pages 1, 2, 3 in order" [ "1"; "2"; "3" ]
              (List.map
                 (Github_fixture.gui_requested_page "pagination")
                 requests);
            List.iter
              (fun (uri, _) ->
                Alcotest.(check (option string))
                  "per_page on every page" (Some "100")
                  (Uri.get_query_param uri "per_page"))
              requests);
        gur_case "non-public entries do not disturb pagination" (fun () ->
            (* Page fullness counts raw entries, not surviving public
               ones: a 100-entry page half full of private repositories
               still advances to the next page. *)
            let privates =
              List.init 50 (fun i ->
                  gur_private_repo
                    ~name:
                      (Printf.sprintf "%s-%d" Github_fixture.gur_private_name i)
                    ~id:(Int64.of_int (9000 + i))
                    ())
            in
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:150 (gur_fill ~from:1000L 50 @ privates);
                  gur_page ~total:150 (gur_fill ~from:2000L 50);
                ]
            in
            let repos = gur_set_exn "private pagination" outcome in
            Alcotest.(check (list int64))
              "only public IDs, in order"
              (Github_fixture.gui_ids ~from:1000L 50
              @ Github_fixture.gui_ids ~from:2000L 50)
              (List.map GUR.repository_id repos);
            Alcotest.(check int) "both pages requested" 2 (List.length requests);
            gur_check_no_private_material repos);
        gur_case "twenty full pages with more indicated hit the limit"
          (fun () ->
            let page n =
              gur_page ~total:2001
                (gur_fill ~from:(Int64.of_int (n * 1000)) 100)
            in
            let outcome, requests =
              gur_list (List.init 20 (fun i -> page (i + 1)))
            in
            gur_expect_error "limit" GUR.Pagination_limit outcome;
            Alcotest.(check (list string))
              "pages 1 through 20, no 21"
              (List.init 20 (fun i -> string_of_int (i + 1)))
              (List.map (Github_fixture.gui_requested_page "limit") requests));
        gur_case "twenty full pages covering the total complete" (fun () ->
            let page n =
              gur_page ~total:2000
                (gur_fill ~from:(Int64.of_int (n * 1000)) 100)
            in
            let outcome, requests =
              gur_list (List.init 20 (fun i -> page (i + 1)))
            in
            let repos = gur_set_exn "exactly 2000" outcome in
            Alcotest.(check int)
              "all two thousand returned" 2000 (List.length repos);
            Alcotest.(check int)
              "exactly twenty requests" 20 (List.length requests));
        gur_case "mid-pagination JSON failure stops immediately" (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:300 (gur_fill ~from:1000L 100);
                  Ok (200, "not json at all");
                ]
            in
            gur_expect_error "mid-scan JSON" GUR.Invalid_response outcome;
            Alcotest.(check int) "exactly two requests" 2 (List.length requests));
      ] )
    (* Transport and status failures: constructors carry at most the
       status integer, the remote body is never parsed or preserved, and
       pagination halts immediately. *);
    ( "github_user_installation_repositories_transport",
      [
        gur_case "transport failure maps to Transport_error" (fun () ->
            let outcome, requests = gur_list [ Error () ] in
            gur_expect_error "transport" GUR.Transport_error outcome;
            Alcotest.(check int) "one request" 1 (List.length requests));
        gur_case "every non-200 preserves only the status integer" (fun () ->
            List.iter
              (fun status ->
                let outcome, requests =
                  gur_list [ Ok (status, "irrelevant") ]
                in
                gur_expect_error
                  ("status " ^ string_of_int status)
                  (GUR.Unexpected_http_status status) outcome;
                Alcotest.(check int) "one request" 1 (List.length requests))
              [ 201; 302; 304; 401; 403; 404; 500; 502 ]);
        gur_case "valid-looking body on a non-200 is not parsed" (fun () ->
            let outcome, _ =
              gur_list
                [
                  Ok
                    ( 401,
                      Github_fixture.gur_body ~total:1
                        [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ]
                    );
                ]
            in
            gur_expect_error "401 with repositories in body"
              (GUR.Unexpected_http_status 401) outcome);
        gur_case "secret-looking response body cannot escape" (fun () ->
            let outcome, _ =
              gur_list [ Ok (502, {|{"secret":"SHOULD-NOT-ESCAPE"}|}) ]
            in
            match outcome with
            | Error e ->
                Alcotest.(check bool)
                  "no body detail in the rendering" false
                  (Html_assert.contains_nonempty ~needle:"SHOULD-NOT-ESCAPE"
                     (Github_fixture.gur_show_error e))
            | Ok _ -> Alcotest.fail "expected Error, got Ok");
        gur_case "mid-pagination transport failure stops immediately" (fun () ->
            let outcome, requests =
              gur_list
                [ gur_page ~total:300 (gur_fill ~from:1000L 100); Error () ]
            in
            gur_expect_error "mid-scan transport" GUR.Transport_error outcome;
            Alcotest.(check int) "exactly two requests" 2 (List.length requests));
        gur_case "mid-pagination status failure stops immediately" (fun () ->
            let outcome, requests =
              gur_list
                [
                  gur_page ~total:300 (gur_fill ~from:1000L 100);
                  Ok (503, "unavailable");
                ]
            in
            gur_expect_error "mid-scan status" (GUR.Unexpected_http_status 503)
              outcome;
            Alcotest.(check int) "exactly two requests" 2 (List.length requests));
      ] )
    (* Token privacy, checked structurally: the token appears in exactly
       one place — the Authorization header — and no returned repository
       or rendered error can carry it. *);
    ( "github_user_installation_repositories_privacy",
      [
        gur_case "token appears only in the Authorization header" (fun () ->
            let _, requests =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            let uri, headers =
              match requests with
              | [ request ] -> request
              | _ -> Alcotest.fail "expected exactly one request"
            in
            Alcotest.(check bool)
              "absent from URI" false
              (Html_assert.contains_nonempty ~needle:gur_access_fixture
                 (Uri.to_string uri));
            List.iter
              (fun (key, value) ->
                if not (String.equal key "authorization") then (
                  Alcotest.(check bool)
                    (key ^ " name is token-free")
                    false
                    (Html_assert.contains_nonempty ~needle:gur_access_fixture
                       key);
                  Alcotest.(check bool)
                    (key ^ " value is token-free")
                    false
                    (Html_assert.contains_nonempty ~needle:gur_access_fixture
                       value)))
              headers);
        gur_case "token never appears in a returned repository" (fun () ->
            let outcome, _ =
              gur_list
                [
                  gur_page ~total:1
                    [ Github_fixture.gur_repo ~id:1001L ~name:"alpha" () ];
                ]
            in
            List.iter
              (fun repo ->
                Alcotest.(check bool)
                  "accessors are token-free" false
                  (Html_assert.contains_nonempty ~needle:gur_access_fixture
                     (gur_repo_strings repo)))
              (gur_set_exn "token privacy" outcome));
        gur_case "rendered errors never contain the token" (fun () ->
            List.iter
              (fun responses ->
                let outcome, _ = gur_list responses in
                match outcome with
                | Error e ->
                    Alcotest.(check bool)
                      "token-free rendering" false
                      (Html_assert.contains_nonempty ~needle:gur_access_fixture
                         (Github_fixture.gur_show_error e))
                | Ok _ -> Alcotest.fail "expected Error, got Ok")
              [
                [ Error () ];
                [ Ok (401, "denied") ];
                [ Ok (200, "not json at all") ];
                [ gur_page ~total:0 [] ];
              ]);
      ] );
  ]
