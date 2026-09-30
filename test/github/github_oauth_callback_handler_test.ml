module Ob = Earde.Project_onboarding
module GOC = Earde.Github_onboarding_crypto
module GCS = Earde.Github_oauth_credentials
module GSD = Earde.Github_onboarding_session_data
module GCK = Earde.Github_onboarding_cookie
module GTE = Earde.Github_oauth_token_exchange
module GUI = Earde.Github_user_installations

(* === GitHub OAuth authorization callback (Github_onboarding_handlers) ===
   GET /integrations/github/authorize/callback: the terminal, sessionless
   leg of onboarding. Every outcome must be a clean 303 with the
   no-store/no-cache/no-referrer headers and an empty body, to exactly
   /projects/new (success, parameter-free — the setup page re-derives the
   viewer's drafts from its own session) or /bring?github=failed — never a
   rendered page, never a distinguishable internal stage. DB-free cases use injected
   mode/config/credentials and fake transports under the fixed test secret;
   no session middleware is ever installed, because the handler must not
   need one. Reaching the missing sql_pool boundary IS the assertion that
   parsing, the cookie gate, and the credentials gate all passed with no
   SQL and no transport call. Raw states, codes, bindings, verifiers,
   secrets, and tokens never reach assertion output — material comparisons
   are boolean. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let case = Case.quick

let path = "/integrations/github/authorize/callback"

let make ~mode ~load_config ~load_credentials ~exchange ~installations
    ~repositories =
  Earde.Github_onboarding_handlers.make_oauth_callback_handler ~mode
    ~load_config ~load_credentials ~exchange_transport:exchange
    ~installations_transport:installations
    ~repositories_transport:repositories

let counting_loader = Http_fixture.counting_loader

let ok_loader = Http_fixture.ok_loader

let status_of = Http_fixture.status_of

let check_safety_headers = Github_handler_fixture.check_safety_headers

let check_deletion = Github_handler_fixture.check_deletion

(* Deterministic printable state fixture, distinct from every other
   suite's. *)
let fixture_state_string = Github_fixture.goc_fixture 'K'

let fixture_state () = Github_fixture.goc_state_exn fixture_state_string

(* Opaque but raw-query-safe authorization code fixture: parsing splits
   on '&' and '#', so those bytes cannot appear, while '=' inside the
   value must survive byte-for-byte. *)
let fixture_code = "cb-c0de.fixture~ok=="

let target_of params = path ^ "?" ^ String.concat "&" params

let code_target =
  target_of [ "state=" ^ fixture_state_string; "code=" ^ fixture_code ]

let error_target =
  target_of [ "state=" ^ fixture_state_string; "error=access_denied" ]

let ok_credentials () = Ok (Github_fixture.gte_credentials ())

let failed_credentials () = Error (GCS.Missing GCS.Client_secret)

(* Success bodies for the fake transports. The token body uses the
   expiring configuration so a refresh token exists and must never leak
   anywhere. *)
let access_fixture = "ghcb-access.TOKEN~1"

let refresh_fixture = "ghcb-refresh.TOKEN~2"

let token_body =
  {|{"access_token":"ghcb-access.TOKEN~1","token_type":"bearer","scope":"","expires_in":28800,"refresh_token":"ghcb-refresh.TOKEN~2","refresh_token_expires_in":15811200}|}

let oauth_rejected_body =
  {|{"error":"bad_verification_code","error_description":"The code passed is incorrect or expired.","error_uri":"https://docs.github.com/x"}|}

(* Counting fake transports. The exchange stub repeats one scripted
   result; the installations stub answers a finite script and fails the
   test outright if over-called. *)
let exchange_stub result calls =
  (module struct
    let post ~uri:_ ~headers:_ ~body:_ =
      incr calls;
      Lwt.return result
  end : GTE.TRANSPORT)

let installations_stub responses calls =
  let remaining = ref responses in
  (module struct
    let get ~uri:_ ~headers:_ =
      incr calls;
      match !remaining with
      | [] -> Alcotest.fail "installations transport over-called"
      | response :: rest ->
          remaining := rest;
          Lwt.return response
  end : GUI.TRANSPORT)

(* A one-page listing that contains [installation_id], via the shared
   gui_entry derivation (account id = id + 1, login "owner-<id>",
   organization target). *)
let listing_for installation_id =
  Ok (200, Github_fixture.gui_body ~total:1 [ installation_id ])

(* Repository fixtures owned by the account the verification fixture
   derives (account id = installation + 1, login "owner-<id>"), so the
   listing client's ownership check accepts them. *)
let repo_owner_login installation = Printf.sprintf "owner-%Ld" installation

let repo_entry ~installation ?private_flag ?visibility ?description
    ?default_branch ?archived ~id ~name () =
  Github_fixture.gur_repo
    ~owner_id:(Int64.add installation 1L)
    ~owner_login:(repo_owner_login installation)
    ?private_flag ?visibility ?description ?default_branch ?archived ~id
    ~name ()

let repo_listing ?total entries =
  Ok
    ( 200,
      Github_fixture.gur_body
        ~total:(Option.value total ~default:(List.length entries))
        entries )

(* One-page listing for the success paths: two public repositories plus
   a private and an internal one that must never be stored anywhere. *)
let default_repo_entries installation =
  [ repo_entry ~installation ~id:501L ~name:"alpha" ()
  ; repo_entry ~installation ~id:502L ~name:"beta"
      ~description:{|"Beta service"|} ()
  ; repo_entry ~installation ~private_flag:true ~visibility:"private"
      ~description:(Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
      ~id:503L ~name:Github_fixture.gur_private_name ()
  ; repo_entry ~installation ~visibility:"internal" ~id:504L
      ~name:"internal-repo" ()
  ]

(* The exact snapshot the default listing must produce: public entries
   only, response order, contiguous positions, flags reset. *)
let default_repo_sigs installation =
  let account_id = Int64.add installation 1L in
  let login = repo_owner_login installation in
  [ Project_fixture.sig_of ~position:1 ~id:501L ~account_id ~login "alpha"
  ; Project_fixture.sig_of ~position:2 ~id:502L ~account_id ~login
      ~description:"Beta service" "beta"
  ]

(* GET transport wrapper logging each call into a shared order list, so
   verification-before-listing is pinned structurally. *)
let logged_get label order (module T : GUI.TRANSPORT) =
  (module struct
    let get ~uri ~headers =
      order := !order @ [ label ];
      T.get ~uri ~headers
  end : GUI.TRANSPORT)

(* DB-free run under the fixed test secret: no session middleware, no
   sql_pool — reaching Dream.sql raises, marking the boundary. Returns
   the outcome plus the credentials/exchange/installations/repositories
   call counts observed by the injected fakes. *)
let gate_run ?(cookies = []) ?(mode = Ob.Public)
    ?(load_config = fun () -> ok_loader ())
    ?(credentials_result = ok_credentials ()) ~target () =
  let cred_calls = ref 0 and ex_calls = ref 0 and inst_calls = ref 0 in
  let repo_captured = ref [] in
  let handler =
    make ~mode ~load_config
      ~load_credentials:(fun () ->
        incr cred_calls;
        credentials_result)
      ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
      ~installations:(installations_stub [ listing_for 12345L ] inst_calls)
      ~repositories:(Github_fixture.gur_transport [ repo_listing [] ] repo_captured)
  in
  let headers =
    match cookies with
    | [] -> []
    | pairs -> [ ("Cookie", Github_fixture.gck_cookie_header pairs) ]
  in
  let request = Dream.request ~method_:`GET ~target ~headers "" in
  let outcome =
    match Lwt_main.run (Dream.set_secret Github_fixture.cookie_secret handler request) with
    | response -> `Response response
    | exception _ -> `Db_boundary
  in
  ( outcome,
    (!cred_calls, !ex_calls, !inst_calls, List.length !repo_captured) )

let gate_response label = function
  | `Response response -> response
  | `Db_boundary -> Alcotest.failf "%s: unexpectedly reached the DB" label

let check_db_boundary label = function
  | `Db_boundary -> ()
  | `Response response ->
      Alcotest.failf "%s: rejected with status %d" label
        (status_of response)

(* The full clean-failure contract; the Location is always the literal
   failure target, so nothing sensitive can be printed. *)
let check_failure label response =
  Alcotest.(check int) (label ^ ": exactly 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location")
    (Some "/bring?github=failed")
    (Dream.header response "Location");
  check_safety_headers label response;
  Alcotest.(check string) (label ^ ": empty body") ""
    (Lwt_main.run (Dream.body response))

let check_no_calls label (cred, ex, inst, repos) =
  Alcotest.(check int) (label ^ ": credentials never loaded") 0 cred;
  Alcotest.(check int) (label ^ ": exchange never called") 0 ex;
  Alcotest.(check int) (label ^ ": installations never called") 0 inst;
  Alcotest.(check int) (label ^ ": repositories never called") 0 repos

(* One parse rejection: clean failure, no deletion cookie, and — because
   parsing precedes configuration — the config loader is never called. *)
let rejected label target =
  let loader, config_calls = counting_loader (ok_loader ()) in
  let outcome, calls = gate_run ~load_config:loader ~target () in
  let response = gate_response label outcome in
  check_failure label response;
  Alcotest.(check int) (label ^ ": no Set-Cookie") 0
    (List.length (Dream.headers response "Set-Cookie"));
  Alcotest.(check int) (label ^ ": config loader never called") 0
    !config_calls;
  check_no_calls label calls

let with_fixture_cookie label f =
  let state = fixture_state () in
  let name, value, _ =
    Github_fixture.gck_stored (label ^ " cookie") (Github_fixture.gck_https_config ()) state
      (GSD.create ())
  in
  f (name, value)

let off_case =
  case "Off: clean failure redirect, nothing else runs" (fun () ->
      let loader, config_calls = counting_loader (ok_loader ()) in
      with_fixture_cookie "off" (fun cookie ->
          let outcome, calls =
            gate_run ~mode:Ob.Off ~load_config:loader ~cookies:[ cookie ]
              ~target:code_target ()
          in
          let response = gate_response "off" outcome in
          check_failure "off" response;
          Alcotest.(check int) "config loader never called" 0 !config_calls;
          check_no_calls "off" calls;
          Alcotest.(check int) "no Set-Cookie" 0
            (List.length (Dream.headers response "Set-Cookie"))))

let state_rejection_case =
  case "strict parsing: state problems redirect cleanly" (fun () ->
      List.iter
        (fun (label, params) -> rejected label (target_of params))
        [ ("missing state", [ "code=" ^ fixture_code ])
        ; ("blank state", [ "state="; "code=" ^ fixture_code ])
        ; ("bare state key", [ "state"; "code=" ^ fixture_code ])
        ; ( "malformed state",
            [ "state=not+canonical"; "code=" ^ fixture_code ] )
        ; ( "padded state",
            [ "state=" ^ fixture_state_string ^ "=";
              "code=" ^ fixture_code ] )
        ; ( "duplicate state",
            [ "state=" ^ fixture_state_string;
              "state=" ^ fixture_state_string; "code=" ^ fixture_code ] )
        ; ( "uppercase State does not satisfy the canonical key",
            [ "State=" ^ fixture_state_string; "code=" ^ fixture_code ] )
        ];
      rejected "no query at all" path;
      rejected "empty query" (path ^ "?"))

let shape_rejection_case =
  case "strict parsing: code/error shape problems redirect cleanly"
    (fun () ->
      List.iter
        (fun (label, params) ->
          rejected label
            (target_of (("state=" ^ fixture_state_string) :: params)))
        [ ("neither code nor error", [])
        ; ( "both code and error",
            [ "code=" ^ fixture_code; "error=access_denied" ] )
        ; ( "duplicate code",
            [ "code=" ^ fixture_code; "code=" ^ fixture_code ] )
        ; ( "duplicate error",
            [ "error=access_denied"; "error=access_denied" ] )
        ; ( "duplicate code beside a valid error",
            [ "code=" ^ fixture_code; "code=" ^ fixture_code;
              "error=access_denied" ] )
        ; ("blank code", [ "code=" ])
        ; ("bare code key", [ "code" ])
        ; ("blank error", [ "error=" ])
        ; ("bare error key", [ "error" ])
        ; ("code with a space is invalid", [ "code=bad code" ])
        ; ("code with a control byte is invalid", [ "code=bad\x01code" ])
        ; ( "uppercase Code does not satisfy the canonical key",
            [ "Code=" ^ fixture_code ] )
        ; ( "uppercase ERROR does not satisfy the canonical key",
            [ "ERROR=access_denied" ] )
        ; ( "error_description alone is not an error",
            [ "error_description=denied" ] )
        ; ( "fragment cuts the query",
            [ "state2=x#code=" ^ fixture_code ] )
        ])

let config_failure_case =
  case "configuration failure: clean failure, nothing later runs"
    (fun () ->
      let loader, config_calls =
        counting_loader (Github_fixture.gac_of_values ~origin:None ())
      in
      with_fixture_cookie "config failure" (fun cookie ->
          let outcome, calls =
            gate_run ~load_config:loader ~cookies:[ cookie ]
              ~target:code_target ()
          in
          let response = gate_response "config failure" outcome in
          check_failure "config failure" response;
          Alcotest.(check int) "loader called once" 1 !config_calls;
          check_no_calls "config failure" calls;
          (* Without configuration there is no cookie policy: no deletion
             may be attempted. *)
          Alcotest.(check int) "no Set-Cookie" 0
            (List.length (Dream.headers response "Set-Cookie"))))

let missing_cookie_case =
  case "missing per-flow cookie: failure with no SQL and no deletion"
    (fun () ->
      let outcome, calls = gate_run ~target:code_target () in
      let response = gate_response "missing cookie" outcome in
      check_failure "missing cookie" response;
      check_no_calls "missing cookie" calls;
      Alcotest.(check int) "no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie"));
      (* Another state's cookie does not satisfy this flow either. *)
      let other = Github_fixture.goc_state_exn (Github_fixture.goc_fixture 'L') in
      let name, value, _ =
        Github_fixture.gck_stored "other flow" (Github_fixture.gck_https_config ()) other (GSD.create ())
      in
      let outcome, calls =
        gate_run ~cookies:[ (name, value) ] ~target:code_target ()
      in
      let response = gate_response "other state's cookie" outcome in
      check_failure "other state's cookie" response;
      check_no_calls "other state's cookie" calls;
      Alcotest.(check int) "still no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie")))

let invalid_cookie_case =
  case "invalid per-flow cookie: failure plus matching deletion, no SQL"
    (fun () ->
      let state = fixture_state () in
      let name, value, _ =
        Github_fixture.gck_stored "victim" (Github_fixture.gck_https_config ()) state (GSD.create ())
      in
      let outcome, calls =
        gate_run ~cookies:[ (name, "AAAA" ^ value) ] ~target:code_target ()
      in
      let response = gate_response "invalid cookie" outcome in
      check_failure "invalid cookie" response;
      check_no_calls "invalid cookie" calls;
      check_deletion "invalid cookie" ~cookie_name:name ~stored_value:value
        response)

let github_rejection_case =
  case "GitHub rejection: cookie deleted, no credentials, SQL, or GitHub"
    (fun () ->
      with_fixture_cookie "rejection" (fun (name, value) ->
          List.iter
            (fun (label, target) ->
              let outcome, calls =
                gate_run ~cookies:[ (name, value) ] ~target ()
              in
              (* A returned response IS the no-SQL proof: consumption
                 would have raised at the missing pool. *)
              let response = gate_response label outcome in
              check_failure label response;
              check_no_calls label calls;
              check_deletion label ~cookie_name:name ~stored_value:value
                response)
            [ ("rejection", error_target)
            ; ( "rejection with remote metadata ignored",
                target_of
                  [ "state=" ^ fixture_state_string; "error=access_denied";
                    "error_description=The+user+denied+access";
                    "error_uri=https%3A%2F%2Fdocs.github.com%2Fx" ] )
            ]))

let credential_failure_case =
  case "credential failure: cookie kept, no SQL, no transport" (fun () ->
      with_fixture_cookie "credentials" (fun cookie ->
          let outcome, (cred, ex, inst, repos) =
            gate_run ~credentials_result:(failed_credentials ())
              ~cookies:[ cookie ] ~target:code_target ()
          in
          let response = gate_response "credential failure" outcome in
          check_failure "credential failure" response;
          Alcotest.(check int) "credentials loader called once" 1 cred;
          Alcotest.(check int) "exchange never called" 0 ex;
          Alcotest.(check int) "installations never called" 0 inst;
          Alcotest.(check int) "repositories never called" 0 repos;
          (* The state is untouched, so the cookie must survive for a
             retry after the deployment is repaired. *)
          Alcotest.(check int) "no Set-Cookie" 0
            (List.length (Dream.headers response "Set-Cookie"))))

let sql_boundary_case =
  case "valid code, cookie, and credentials reach consumption first"
    (fun () ->
      with_fixture_cookie "boundary" (fun cookie ->
          (* No session middleware is installed anywhere in this suite;
             reaching the SQL boundary in both modes proves no session
             field gated the way, and the still-zero transport counters
             prove consumption strictly precedes any GitHub call. *)
          List.iter
            (fun (label, mode) ->
              let outcome, (cred, ex, inst, repos) =
                gate_run ~mode ~cookies:[ cookie ] ~target:code_target ()
              in
              check_db_boundary label outcome;
              Alcotest.(check int)
                (label ^ ": credentials loaded once")
                1 cred;
              Alcotest.(check int) (label ^ ": exchange not yet called") 0
                ex;
              Alcotest.(check int)
                (label ^ ": installations not yet called")
                0 inst;
              Alcotest.(check int)
                (label ^ ": repositories not yet called")
                0 repos)
            [ ("Public", Ob.Public); ("Admins", Ob.Admins) ]))

let unrelated_params_case =
  case "unrelated extra query parameters are accepted" (fun () ->
      with_fixture_cookie "extras" (fun cookie ->
          let outcome, _ =
            gate_run ~cookies:[ cookie ]
              ~target:
                (target_of
                   [ "ref=x"; "state=" ^ fixture_state_string;
                     "setup_action=install"; "State=UP"; "bare";
                     "code=" ^ fixture_code; "code2=9";
                     "error_description=ignored"; "error_uri=ignored" ])
              ()
          in
          check_db_boundary "extras ignored" outcome))

let no_leakage_case =
  case "failure responses never echo state, code, or error values"
    (fun () ->
      List.iter
        (fun (label, target) ->
          let outcome, _ = gate_run ~target () in
          let response = gate_response label outcome in
          let body = Lwt_main.run (Dream.body response) in
          let location =
            Option.value (Dream.header response "Location") ~default:""
          in
          List.iter
            (fun needle ->
              Alcotest.(check bool) (label ^ ": absent from the body")
                false
                (Html_assert.contains_nonempty ~needle body);
              Alcotest.(check bool)
                (label ^ ": absent from the Location")
                false
                (Html_assert.contains_nonempty ~needle location))
            [ fixture_state_string; fixture_code; "access_denied" ])
        [ ("code shape", code_target); ("error shape", error_target) ])

let gate_suite =
  [ off_case; state_rejection_case; shape_rejection_case;
    config_failure_case; missing_cookie_case; invalid_cookie_case;
    github_rejection_case; credential_failure_case; sql_boundary_case;
    unrelated_params_case; no_leakage_case ]

(* --- Database-gated: the real start + setup-return flow first, then
   the callback over sql_pool + secret, still with no session middleware
   and with fake transports only. --- *)

let or_fail = Github_handler_fixture.or_fail

(* Reserved installation-id range for this suite: 936000001-936000999
   (the installation-store suite owns 935xxx). *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ (* The draft-failure case's injected trigger, in case a mid-case
         failure left it behind. *)
      "DROP TRIGGER IF EXISTS ghoauth_fail_insert \
       ON project_onboarding_draft_repositories"
    ; "DROP FUNCTION IF EXISTS ghoauth_fail_insert_fn()"
    ; "DELETE FROM github_onboarding_states WHERE user_id IN \
       (SELECT id FROM users WHERE username LIKE 'ghoauth_%')"
      (* Drafts first: installations are RESTRICT-protected while
         referenced; snapshots cascade from drafts. *)
    ; "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 936000001 AND 936000999)"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 936000001 AND 936000999"
    ; "DELETE FROM users WHERE username LIKE 'ghoauth_%'"
    ]

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   let* () = or_fail "cleanup" r in
                   Lwt.return_unit)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 (* Unlike the older suites this one runs many requests,
                    so it must not leak its per-case connection. *)
                 let* () = cleanup () in
                 C.disconnect ())))

let state_hash_of = Github_handler_fixture.state_hash_of

let q_state_row =
  (Caqti_type.(string ->! t2 (option int64) bool))
  "SELECT pending_github_installation_id, consumed_at IS NULL
   FROM github_onboarding_states WHERE state_hash = $1"

let q_installation_row =
  (Caqti_type.(int64 ->! t2 (t3 int64 string string) (option int)))
  "SELECT github_account_id, github_account_login, github_account_type,
          connected_by_user_id
   FROM github_installations WHERE github_installation_id = $1"

let q_installation_count =
  (Caqti_type.int64 ->! Caqti_type.int)
  "SELECT COUNT(*) FROM github_installations
   WHERE github_installation_id = $1"

(* Full rows as one text value each, for boolean secret-absence sweeps
   over everything either table stores. *)
let q_installation_text =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT ROW(t.*)::text FROM github_installations t
   WHERE t.github_installation_id = $1"

let q_state_text =
  (Caqti_type.string ->! Caqti_type.string)
  "SELECT ROW(t.*)::text FROM github_onboarding_states t
   WHERE t.state_hash = $1"

let q_installation_record_id =
  (Caqti_type.int64 ->! Caqti_type.int64)
  "SELECT id FROM github_installations WHERE github_installation_id = $1"

let q_installation_status =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT status FROM github_installations
   WHERE github_installation_id = $1"

let q_draft_text =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT ROW(t.*)::text FROM project_onboarding_drafts t WHERE t.id = $1"

let q_snapshot_text =
  (Caqti_type.int64 ->! Caqti_type.string)
  "SELECT COALESCE(string_agg(ROW(t.*)::text, '|'), '')
   FROM project_onboarding_draft_repositories t WHERE t.draft_id = $1"

(* Deterministic draft-store failure for the partial-persistence case:
   the Pod_store trigger mechanism under suite-local names, scoped to one
   reserved repository id, installed and dropped inside that case
   alone. *)
let poison_repo_id = 936999999L

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE FUNCTION ghoauth_fail_insert_fn() RETURNS trigger
   LANGUAGE plpgsql
   AS 'BEGIN RAISE EXCEPTION ''ghoauth fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "CREATE TRIGGER ghoauth_fail_insert
   BEFORE INSERT ON project_onboarding_draft_repositories
   FOR EACH ROW WHEN (NEW.github_repository_id = 936999999)
   EXECUTE FUNCTION ghoauth_fail_insert_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP TRIGGER IF EXISTS ghoauth_fail_insert
   ON project_onboarding_draft_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
  "DROP FUNCTION IF EXISTS ghoauth_fail_insert_fn()"

(* Future-dated lifetime: the row stays live (unexpired, unconsumed),
   but consume's UPDATE of consumed_at = NOW() then violates the
   consumed_after_created CHECK — a real storage error at the handler
   boundary without weakening any production constraint. *)
let q_future =
  (Caqti_type.string ->. Caqti_type.unit)
  "UPDATE github_onboarding_states
   SET created_at = NOW() + INTERVAL '1 hour',
       expires_at = NOW() + INTERVAL '2 hours'
   WHERE state_hash = $1"

(* Pre-existing terminally revoked row, so a later verified persistence
   of the same installation id must fail without modifying it. *)
let q_insert_revoked =
  (Caqti_type.(t2 int64 int64 ->. unit))
  "INSERT INTO github_installations
     (github_installation_id, github_account_id, github_account_login,
      github_account_type, status, revoked_at)
   VALUES ($1, $2, 'revoked-owner', 'organization', 'revoked', NOW())"

(* Pre-existing ACTIVE personal-account row, so a later organization
   verification of the same installation id must be refused as an
   identity change rather than silently rewriting the kind. *)
let q_insert_active_user =
  (Caqti_type.(t2 int64 int64 ->. unit))
  "INSERT INTO github_installations
     (github_installation_id, github_account_id, github_account_login,
      github_account_type, status)
   VALUES ($1, $2, 'personal-owner-fixture', 'user', 'active')"

let fixture_user (module C : Caqti_lwt.CONNECTION) name =
  let* uid = C.find Github_handler_fixture.q_insert_user name in
  or_fail "fixture user" uid

(* One shared single-connection sql_pool pipeline for the whole suite:
   nothing ever closes a Dream.sql_pool, and this suite runs enough
   requests (start + setup return + callback per case) that a fresh pool
   per request would exhaust Postgres max_connections. The inner handler
   is swapped per request; cases run sequentially. *)
let shared_handler : Dream.handler ref =
  ref (fun _ -> Alcotest.fail "no handler installed")

let shared_pipeline = ref None

let run_shared ~url handler request =
  let pipeline =
    match !shared_pipeline with
    | Some pipeline -> pipeline
    | None ->
        let pipeline =
          Dream.sql_pool ~size:1 url
          @@ Dream.set_secret Github_fixture.cookie_secret
          @@ fun req -> !shared_handler req
        in
        shared_pipeline := Some pipeline;
        pipeline
  in
  shared_handler := handler;
  pipeline request

let cookie_jar_headers = function
  | [] -> []
  | pairs -> [ ("Cookie", Github_fixture.gck_cookie_header pairs) ]

(* Lwt-native gck_stored: db cases already run inside Lwt_main.run, so
   crafting a cookie (e.g. the forged-binding one) must compose instead
   of nesting run. *)
let stored_lwt label config state data =
  let headers = ref None in
  let* (_ : Dream.response) =
    Dream.set_secret Github_fixture.cookie_secret
      (fun request ->
        let response = Dream.response "" in
        GCK.store config ~request ~response ~state data;
        headers := Some (Dream.headers response "Set-Cookie");
        Dream.respond "")
      (Dream.request "")
  in
  match !headers with
  | Some [ header ] ->
      let name, value, _ = Github_fixture.gck_parse_set_cookie header in
      Lwt.return (name, value)
  | _ -> Alcotest.failf "%s: expected exactly one stored Set-Cookie" label

(* The real preceding flow over the shared pool: the authenticated start
   (memory session for the fixture user), then the setup return attaching
   [installation]; yields the state plus the untouched per-flow cookie
   exactly as the browser holds them. *)
let onboard ~url ~uid ~installation label =
  let start_handler =
    Dream.memory_sessions (fun req ->
        let* () =
          Dream.set_session_field req "user_id" (string_of_int uid)
        in
        Earde.Github_onboarding_handlers.make_start_installation_handler
          ~mode:Ob.Public
          ~load_config:(fun () -> ok_loader ())
          req)
  in
  let* response =
    run_shared ~url start_handler
      (Dream.request ~method_:`POST
         ~target:"/integrations/github/install/start"
         ~headers:[ ("Origin", "https://earde.com") ]
         "")
  in
  let _, state, name, value =
    Github_handler_fixture.successful_start (label ^ ": start") response
  in
  let cookie = (name, value) in
  let return_handler =
    Github_handler_fixture.make_setup_return_handler ~mode:Ob.Public ~load_config:(fun () ->
        ok_loader ())
  in
  let* response =
    run_shared ~url return_handler
      (Dream.request ~method_:`GET
         ~target:
           (Github_handler_fixture.target_of
              [ "state=" ^ GOC.state_to_string state;
                "installation_id=" ^ Int64.to_string installation ])
         ~headers:(cookie_jar_headers [ cookie ])
         "")
  in
  Alcotest.(check int) (label ^ ": setup return 303") 303
    (status_of response);
  (match Dream.header response "Location" with
  | Some location ->
      Alcotest.(check bool) (label ^ ": setup return reached GitHub") true
        (Html_assert.contains_nonempty ~needle:"github.com" location)
  | None -> Alcotest.fail (label ^ ": setup return had no Location"));
  Lwt.return (state, cookie)

let callback_target state code =
  target_of [ "state=" ^ GOC.state_to_string state; "code=" ^ code ]

(* The callback over the real pipeline: shared sql_pool + secret,
   deliberately no session middleware and no session fields. *)
let run_callback ~url ?(jar = []) ?(credentials = ok_credentials ())
    ~exchange ~installations ~repositories ~target () =
  let handler =
    make ~mode:Ob.Public
      ~load_config:(fun () -> ok_loader ())
      ~load_credentials:(fun () -> credentials)
      ~exchange ~installations ~repositories
  in
  run_shared ~url handler
    (Dream.request ~method_:`GET ~target
       ~headers:(cookie_jar_headers jar)
       "")

(* Lwt-safe response contracts. *)
let check_redirect_lwt label ~location response =
  Alcotest.(check int) (label ^ ": exactly 303") 303 (status_of response);
  Alcotest.(check (option string)) (label ^ ": Location") (Some location)
    (Dream.header response "Location");
  check_safety_headers label response;
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

let check_failure_lwt label response =
  check_redirect_lwt label ~location:"/bring?github=failed" response

(* The success continuation is the literal, parameter-free setup page:
   equality with the fixed string is also the proof that no draft id,
   installation id, repository id, state, or credential rides along. *)
let check_success_lwt label response =
  check_redirect_lwt label ~location:"/projects/new" response

let state_row (module C : Caqti_lwt.CONNECTION) label state =
  let* row = C.find q_state_row (state_hash_of state) in
  or_fail (label ^ ": state row") row

let installation_count (module C : Caqti_lwt.CONNECTION) label id =
  let* count = C.find q_installation_count id in
  or_fail (label ^ ": installation count") count

let installation_record_id (module C : Caqti_lwt.CONNECTION) label id =
  let* rid = C.find q_installation_record_id id in
  or_fail (label ^ ": installation record id") rid

(* Draft observations reuse the Pod_store queries and its snapshot
   signature format, so both suites pin the identical stored shape. *)
let active_draft (module C : Caqti_lwt.CONNECTION) label ~uid ~record_id =
  let* draft = C.find_opt Project_fixture.q_active_draft_id (uid, record_id) in
  or_fail (label ^ ": active draft") draft

let require_draft label = function
  | Some id -> id
  | None -> Alcotest.failf "%s: no active draft" label

let draft_count (module C : Caqti_lwt.CONNECTION) label uid =
  let* count = C.find Project_fixture.q_count_for_user uid in
  or_fail (label ^ ": draft count") count

let active_draft_count (module C : Caqti_lwt.CONNECTION) label uid =
  let* count = C.find Project_fixture.q_count_active_for_user uid in
  or_fail (label ^ ": active draft count") count

let snapshot_sigs (module C : Caqti_lwt.CONNECTION) label draft_id =
  let* sigs = C.collect_list Project_fixture.q_sigs draft_id in
  or_fail (label ^ ": snapshot signatures") sigs

(* One terminal failure: generic redirect plus this flow's cookie
   deletion, and no verified installation row. *)
let check_terminal_failure (module C : Caqti_lwt.CONNECTION) label
    ~cookie ~installation response =
  let* () = check_failure_lwt label response in
  check_deletion label ~cookie_name:(fst cookie)
    ~stored_value:(snd cookie) response;
  let* count = installation_count (module C) label installation in
  Alcotest.(check int) (label ^ ": no installation row") 0 count;
  Lwt.return_unit

let success_case =
  db_case
    "successful callback: consume + exchange + verify + list + persist + \
     draft + clean redirect"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_user" in
      let installation = 936000001L in
      let config = Github_fixture.gck_https_config () in
      let* state, cookie = onboard ~url ~uid ~installation "success" in
      let* data = Github_handler_fixture.load_cookie config state [ cookie ] in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] and order = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:
            (logged_get "verify" order
               (installations_stub [ listing_for installation ] inst_calls))
          ~repositories:
            (logged_get "list" order
               (Github_fixture.gur_transport
                  [ repo_listing (default_repo_entries installation) ]
                  repo_captured))
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_success_lwt "success" response in
      Alcotest.(check int) "one token exchange" 1 !ex_calls;
      Alcotest.(check int) "one installation listing" 1 !inst_calls;
      Alcotest.(check int) "one repositories listing" 1
        (List.length !repo_captured);
      Alcotest.(check (list string)) "listing only after verification"
        [ "verify"; "list" ] !order;
      (* The repositories request carried the exchanged token and the
         verified installation's fixed path — the only way either can
         reach the listing client. *)
      (match !repo_captured with
      | [ (uri, headers) ] ->
          Alcotest.(check bool) "listing path is the installation's" true
            (Html_assert.contains_nonempty
               ~needle:
                 (Printf.sprintf "/user/installations/%Ld/repositories"
                    installation)
               (Uri.to_string uri));
          Alcotest.(check bool) "listing bears the exchanged token" true
            (List.exists
               (fun (k, v) ->
                 String.equal (String.lowercase_ascii k) "authorization"
                 && String.equal v ("Bearer " ^ access_fixture))
               headers)
      | _ -> Alcotest.fail "expected exactly one repositories request");
      (* The flow's cookie is deleted, and only it — memoryless of any
         session because none exists. *)
      check_deletion "success" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) response;
      (* State consumed; the untrusted pending id was exactly what got
         verified and persisted. *)
      let* pending, consumed_null = state_row (module C) "success" state in
      Alcotest.(check bool) "pending id retained" true
        (pending = Some installation);
      Alcotest.(check bool) "state consumed" false consumed_null;
      let* row = C.find q_installation_row installation in
      let* (account_id, login, account_type), connected_by =
        or_fail "installation row" row
      in
      (* Identity exactly as the verification response derived it
         (gui_entry: account id = id + 1, login "owner-<id>",
         organization). *)
      Alcotest.(check bool) "account id from verification" true
        (account_id = Int64.add installation 1L);
      Alcotest.(check string) "account login from verification"
        (Printf.sprintf "owner-%Ld" installation)
        login;
      Alcotest.(check string) "organization target" "organization"
        account_type;
      (* The connected user is the start-handler fixture user, read back
         from the consumed state — no session existed at the callback. *)
      Alcotest.(check (option int)) "connected by the stored owner"
        (Some uid) connected_by;
      let* count = installation_count (module C) "success" installation in
      Alcotest.(check int) "exactly one installation row" 1 count;
      (* Exactly one active draft, owned by the consumed state's user and
         referencing the local installation record, whose snapshot is
         exactly the listed public repositories in response order with
         both selection flags reset. *)
      let* record_id =
        installation_record_id (module C) "success" installation
      in
      let* drafts = draft_count (module C) "success" uid in
      Alcotest.(check int) "exactly one draft" 1 drafts;
      let* active = active_draft_count (module C) "success" uid in
      Alcotest.(check int) "exactly one active draft" 1 active;
      let* draft = active_draft (module C) "success" ~uid ~record_id in
      let draft = require_draft "success" draft in
      let* sigs = snapshot_sigs (module C) "success" draft in
      Alcotest.(check (list string))
        "snapshot is exactly the public listing"
        (default_repo_sigs installation)
        sigs;
      (* Privacy sweep: nothing secret in any stored text value of any
         row — tokens, code, raw state, raw binding, raw verifier, client
         secret, private-repository metadata. *)
      let* installation_text = C.find q_installation_text installation in
      let* installation_text =
        or_fail "installation text" installation_text
      in
      let* state_text = C.find q_state_text (state_hash_of state) in
      let* state_text = or_fail "state text" state_text in
      let* draft_text = C.find q_draft_text draft in
      let* draft_text = or_fail "draft text" draft_text in
      let* snapshot_text = C.find q_snapshot_text draft in
      let* snapshot_text = or_fail "snapshot text" snapshot_text in
      let stored =
        String.concat "|"
          [ installation_text; state_text; draft_text; snapshot_text ]
      in
      let raw_binding, raw_verifier =
        match String.split_on_char '.' (GSD.encode data) with
        | [ _version; binding; verifier ] -> (binding, verifier)
        | _ -> Alcotest.fail "unexpected cookie plaintext shape"
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) "absent from every stored text value"
            false
            (Html_assert.contains_nonempty ~needle stored))
        [ access_fixture; refresh_fixture; fixture_code;
          GOC.state_to_string state; raw_binding; raw_verifier;
          Github_fixture.gte_client_secret; Github_fixture.gur_private_name; Github_fixture.gur_private_description ];
      (* And nothing sensitive in the response itself. *)
      let location =
        Option.value (Dream.header response "Location") ~default:""
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool) "absent from the Location" false
            (Html_assert.contains_nonempty ~needle location))
        [ GOC.state_to_string state; fixture_code; access_fixture;
          refresh_fixture; Int64.to_string installation ];
      Lwt.return_unit)

let replay_case =
  db_case "replay after success: generic failure, no second effect"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_replay" in
      let installation = 936000002L in
      let* state, cookie = onboard ~url ~uid ~installation "replay" in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let exchange = exchange_stub (Ok (200, token_body)) ex_calls in
      let installations =
        installations_stub [ listing_for installation ] inst_calls
      in
      (* One scripted full listing: a second listing attempt would
         over-call the transport and fail the test outright. *)
      let repositories =
        Github_fixture.gur_transport
          [ repo_listing (default_repo_entries installation) ]
          repo_captured
      in
      let target = callback_target state fixture_code in
      let* first =
        run_callback ~url ~jar:[ cookie ] ~exchange ~installations
          ~repositories ~target ()
      in
      let* () = check_success_lwt "first callback" first in
      (* Identical replay with the original cookie value. *)
      let* second =
        run_callback ~url ~jar:[ cookie ] ~exchange ~installations
          ~repositories ~target ()
      in
      let* () = check_failure_lwt "replay" second in
      check_deletion "replay" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) second;
      Alcotest.(check int) "no second token exchange" 1 !ex_calls;
      Alcotest.(check int) "no second listing" 1 !inst_calls;
      Alcotest.(check int) "no second repositories listing" 1
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) "replay" state in
      Alcotest.(check bool) "state remains consumed" false consumed_null;
      let* count = installation_count (module C) "replay" installation in
      Alcotest.(check int) "still one installation row" 1 count;
      (* Exactly one active draft with its one complete snapshot. *)
      let* record_id =
        installation_record_id (module C) "replay" installation
      in
      let* drafts = draft_count (module C) "replay" uid in
      Alcotest.(check int) "still one draft" 1 drafts;
      let* draft = active_draft (module C) "replay" ~uid ~record_id in
      let draft = require_draft "replay" draft in
      let* sigs = snapshot_sigs (module C) "replay" draft in
      Alcotest.(check (list string)) "snapshot still complete"
        (default_repo_sigs installation)
        sigs;
      Lwt.return_unit)

(* A consume-stage terminal failure: transports must never run and the
   cookie is deleted. [prepare] mutates the issued state row first. *)
let consume_failure_case name ~username ~installation ~prepare
    ~check_consumed =
  db_case name (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) username in
      let* state, cookie = onboard ~url ~uid ~installation name in
      let* () = prepare (module C : Caqti_lwt.CONNECTION) state in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) name ~cookie ~installation
          response
      in
      Alcotest.(check int) (name ^ ": exchange never called") 0 !ex_calls;
      Alcotest.(check int)
        (name ^ ": installations never called")
        0 !inst_calls;
      Alcotest.(check int)
        (name ^ ": repositories never called")
        0
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) name state in
      check_consumed consumed_null;
      Lwt.return_unit)

let expired_case =
  consume_failure_case "expired state: failure, no transport, no burn"
    ~username:"ghoauth_expired" ~installation:936000003L
    ~prepare:(fun (module C : Caqti_lwt.CONNECTION) state ->
      let* r = C.exec Github_handler_fixture.q_expire (state_hash_of state) in
      or_fail "expire" r)
    ~check_consumed:(fun consumed_null ->
      Alcotest.(check bool) "expired row left unconsumed" true
        consumed_null)

let already_consumed_case =
  consume_failure_case
    "already-consumed state: failure, no transport, stays consumed"
    ~username:"ghoauth_consumed" ~installation:936000004L
    ~prepare:(fun (module C : Caqti_lwt.CONNECTION) state ->
      let* r = C.exec Github_handler_fixture.q_consume_now (state_hash_of state) in
      or_fail "consume" r)
    ~check_consumed:(fun consumed_null ->
      Alcotest.(check bool) "row stays consumed" false consumed_null)

let binding_mismatch_case =
  db_case "session-binding mismatch: durable burn, no transport"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_mismatch" in
      let installation = 936000005L in
      let* state, cookie = onboard ~url ~uid ~installation "mismatch" in
      (* A forged cookie for the same state carrying fresh (wrong)
         material: decrypts fine, but its binding hash cannot match the
         issued row, so consume burns the state. *)
      let* forged_name, forged_value =
        stored_lwt "forged" (Github_fixture.gck_https_config ()) state (GSD.create ())
      in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url
          ~jar:[ (forged_name, forged_value) ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) "mismatch"
          ~cookie:(forged_name, forged_value) ~installation response
      in
      Alcotest.(check int) "exchange never called" 0 !ex_calls;
      Alcotest.(check int) "installations never called" 0 !inst_calls;
      Alcotest.(check int) "repositories never called" 0
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) "mismatch" state in
      Alcotest.(check bool) "state durably burned" false consumed_null;
      (* The burn is terminal: the genuine cookie cannot finish either. *)
      let* retry =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_failure_lwt "post-burn retry" retry in
      Alcotest.(check int) "still no exchange" 0 !ex_calls;
      Lwt.return_unit)

(* An exchange-stage terminal failure: verification and persistence must
   never run; the state is already consumed. *)
let exchange_failure_case name ~username ~installation ~exchange_result =
  db_case name (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) username in
      let* state, cookie = onboard ~url ~uid ~installation name in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub exchange_result ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) name ~cookie ~installation
          response
      in
      Alcotest.(check int) (name ^ ": one exchange attempt") 1 !ex_calls;
      Alcotest.(check int)
        (name ^ ": verification never ran")
        0 !inst_calls;
      Alcotest.(check int)
        (name ^ ": repositories never called")
        0
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) name state in
      Alcotest.(check bool) (name ^ ": state consumed first") false
        consumed_null;
      Lwt.return_unit)

let exchange_transport_case =
  exchange_failure_case "exchange transport failure: terminal"
    ~username:"ghoauth_extransport" ~installation:936000006L
    ~exchange_result:(Error ())

let oauth_rejected_case =
  exchange_failure_case "OAuth-rejected token response: terminal"
    ~username:"ghoauth_exrejected" ~installation:936000007L
    ~exchange_result:(Ok (200, oauth_rejected_body))

(* A verification-stage terminal failure: persistence must never run. *)
let verification_failure_case name ~username ~installation ~responses
    ~expected_listing_calls =
  db_case name (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) username in
      let* state, cookie = onboard ~url ~uid ~installation name in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub responses inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) name ~cookie ~installation
          response
      in
      Alcotest.(check int) (name ^ ": one exchange") 1 !ex_calls;
      Alcotest.(check int)
        (name ^ ": listing requests")
        expected_listing_calls !inst_calls;
      Alcotest.(check int)
        (name ^ ": repositories never called")
        0
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) name state in
      Alcotest.(check bool) (name ^ ": state consumed first") false
        consumed_null;
      Lwt.return_unit)

let not_accessible_case =
  verification_failure_case
    "installation not accessible: terminal, nothing persisted"
    ~username:"ghoauth_inaccessible" ~installation:936000008L
    ~responses:[ Ok (200, Github_fixture.gui_body ~total:1 [ 936000998L ]) ]
    ~expected_listing_calls:1

let pagination_limit_case =
  verification_failure_case
    "pagination limit: terminal, nothing persisted"
    ~username:"ghoauth_pagination" ~installation:936000009L
    ~responses:
      (List.init 5 (fun page ->
           Github_fixture.gui_page_of ~total:600
             (Github_fixture.gui_ids ~from:(Int64.of_int (1000 + (page * 100))) 100)))
    ~expected_listing_calls:5

let persistence_failure_case =
  db_case
    "revoked installation: persistence fails only after every stage"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_revoked" in
      let installation = 936000010L in
      (* Terminal revoked row for the same installation id, with the
         account identity the verification response will carry. *)
      let* r =
        C.exec q_insert_revoked (installation, Int64.add installation 1L)
      in
      let* () = or_fail "revoked fixture" r in
      let* state, cookie = onboard ~url ~uid ~installation "revoked" in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:
            (installations_stub [ listing_for installation ] inst_calls)
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing (default_repo_entries installation) ]
               repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_failure_lwt "revoked" response in
      check_deletion "revoked" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) response;
      (* Persistence was attempted last: consume, exchange, verification,
         and the repository listing all really ran first. *)
      Alcotest.(check int) "one exchange" 1 !ex_calls;
      Alcotest.(check int) "one listing" 1 !inst_calls;
      Alcotest.(check int) "one repositories listing" 1
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) "revoked" state in
      Alcotest.(check bool) "state consumed" false consumed_null;
      (* The revoked row is untouched — still revoked, still alone. *)
      let* row = C.find q_installation_row installation in
      let* (_, login, _), _ = or_fail "revoked row" row in
      Alcotest.(check string) "revoked row untouched" "revoked-owner"
        login;
      let* count = installation_count (module C) "revoked" installation in
      Alcotest.(check int) "still exactly one row" 1 count;
      (* The draft store never ran: no draft, hence no snapshot. *)
      let* drafts = draft_count (module C) "revoked" uid in
      Alcotest.(check int) "no draft" 0 drafts;
      Lwt.return_unit)

let consume_storage_error_case =
  db_case "consume storage error: cookie kept, no transport" (fun ~url
      (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_storage" in
      let installation = 936000011L in
      let* state, cookie = onboard ~url ~uid ~installation "storage" in
      let* r = C.exec q_future (state_hash_of state) in
      let* () = or_fail "future-date" r in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_failure_lwt "storage error" response in
      (* No committed outcome: the cookie must survive for a retry. *)
      Alcotest.(check int) "no Set-Cookie" 0
        (List.length (Dream.headers response "Set-Cookie"));
      Alcotest.(check int) "exchange never called" 0 !ex_calls;
      Alcotest.(check int) "installations never called" 0 !inst_calls;
      Alcotest.(check int) "repositories never called" 0
        (List.length !repo_captured);
      let* count =
        installation_count (module C) "storage error" installation
      in
      Alcotest.(check int) "nothing persisted" 0 count;
      Lwt.return_unit)

let parallel_case =
  db_case "parallel flows: completing A leaves B fully intact" (fun ~url
      (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_parallel" in
      let installation_a = 936000012L and installation_b = 936000013L in
      let config = Github_fixture.gck_https_config () in
      let entries_a =
        [ repo_entry ~installation:installation_a ~id:511L ~name:"only-a"
            ()
        ]
      in
      let sigs_a =
        [ Project_fixture.sig_of ~position:1 ~id:511L
            ~account_id:(Int64.add installation_a 1L)
            ~login:(repo_owner_login installation_a)
            "only-a"
        ]
      in
      let entries_b =
        [ repo_entry ~installation:installation_b ~id:521L ~name:"only-b"
            ()
        ]
      in
      let sigs_b =
        [ Project_fixture.sig_of ~position:1 ~id:521L
            ~account_id:(Int64.add installation_b 1L)
            ~login:(repo_owner_login installation_b)
            "only-b"
        ]
      in
      let* state_a, cookie_a =
        onboard ~url ~uid ~installation:installation_a "flow A"
      in
      let* state_b, cookie_b =
        onboard ~url ~uid ~installation:installation_b "flow B"
      in
      Alcotest.(check bool) "distinct cookie names" false
        (String.equal (fst cookie_a) (fst cookie_b));
      let jar = [ cookie_a; cookie_b ] in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let* response =
        run_callback ~url ~jar
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:
            (installations_stub [ listing_for installation_a ] inst_calls)
          ~repositories:(Github_fixture.gur_transport [ repo_listing entries_a ] (ref []))
          ~target:(callback_target state_a fixture_code)
          ()
      in
      let* () = check_success_lwt "flow A" response in
      (* Only A's cookie is deleted. *)
      check_deletion "flow A" ~cookie_name:(fst cookie_a)
        ~stored_value:(snd cookie_a) response;
      (* Only A's state is consumed; only A's installation exists. *)
      let* pending_a, consumed_a = state_row (module C) "row A" state_a in
      Alcotest.(check bool) "A pending kept" true
        (pending_a = Some installation_a);
      Alcotest.(check bool) "A consumed" false consumed_a;
      let* pending_b, consumed_b = state_row (module C) "row B" state_b in
      Alcotest.(check bool) "B pending kept" true
        (pending_b = Some installation_b);
      Alcotest.(check bool) "B unconsumed" true consumed_b;
      let* count_a =
        installation_count (module C) "flow A" installation_a
      in
      Alcotest.(check int) "A persisted" 1 count_a;
      let* count_b =
        installation_count (module C) "flow B" installation_b
      in
      Alcotest.(check int) "B not persisted" 0 count_b;
      (* B's cookie still carries B's own material — no crossover. *)
      let* data_a = Github_handler_fixture.load_cookie config state_a jar in
      let* data_b = Github_handler_fixture.load_cookie config state_b jar in
      Alcotest.(check bool) "distinct verifiers" false
        (String.equal (Github_fixture.gsd_verifier data_a) (Github_fixture.gsd_verifier data_b));
      Alcotest.(check bool) "distinct bindings" false
        (String.equal (Github_fixture.gsd_binding_hash data_a) (Github_fixture.gsd_binding_hash data_b));
      (* A's draft carries exactly A's repository set, and it is the only
         draft so far. *)
      let* record_a =
        installation_record_id (module C) "record A" installation_a
      in
      let* draft_a =
        active_draft (module C) "draft A" ~uid ~record_id:record_a
      in
      let draft_a = require_draft "draft A" draft_a in
      let* stored_a = snapshot_sigs (module C) "draft A" draft_a in
      Alcotest.(check (list string)) "A's snapshot is set A" sigs_a
        stored_a;
      let* active = active_draft_count (module C) "after A" uid in
      Alcotest.(check int) "one active draft after A" 1 active;
      (* Completing B later still succeeds independently, producing its
         own draft under its own installation record and snapshot. *)
      let* response_b =
        run_callback ~url ~jar
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for installation_b ] (ref 0))
          ~repositories:(Github_fixture.gur_transport [ repo_listing entries_b ] (ref []))
          ~target:(callback_target state_b fixture_code)
          ()
      in
      let* () = check_success_lwt "flow B" response_b in
      check_deletion "flow B" ~cookie_name:(fst cookie_b)
        ~stored_value:(snd cookie_b) response_b;
      let* record_b =
        installation_record_id (module C) "record B" installation_b
      in
      let* draft_b =
        active_draft (module C) "draft B" ~uid ~record_id:record_b
      in
      let draft_b = require_draft "draft B" draft_b in
      Alcotest.(check bool) "distinct drafts" false
        (Int64.equal draft_a draft_b);
      let* stored_b = snapshot_sigs (module C) "draft B" draft_b in
      Alcotest.(check (list string)) "B's snapshot is set B" sigs_b
        stored_b;
      (* A's draft and snapshot survive B's completion untouched. *)
      let* stored_a = snapshot_sigs (module C) "draft A after B" draft_a in
      Alcotest.(check (list string)) "A's snapshot unchanged" sigs_a
        stored_a;
      let* active = active_draft_count (module C) "after B" uid in
      Alcotest.(check int) "two active drafts, one per installation" 2
        active;
      Lwt.return_unit)

(* A repository-listing terminal failure: state already consumed, cookie
   deleted, and — because listing precedes the second SQL scope — no
   installation row and no draft may exist afterwards. *)
let listing_failure_case name ~username ~installation ~responses
    ~expected_listing_calls =
  db_case name (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) username in
      let* state, cookie = onboard ~url ~uid ~installation name in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:
            (installations_stub [ listing_for installation ] inst_calls)
          ~repositories:(Github_fixture.gur_transport responses repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) name ~cookie ~installation
          response
      in
      Alcotest.(check int) (name ^ ": one exchange") 1 !ex_calls;
      Alcotest.(check int) (name ^ ": one verification") 1 !inst_calls;
      Alcotest.(check int)
        (name ^ ": repository listing requests")
        expected_listing_calls
        (List.length !repo_captured);
      let* _, consumed_null = state_row (module C) name state in
      Alcotest.(check bool) (name ^ ": state consumed") false consumed_null;
      (* No draft store call happened: no draft, hence no snapshot. *)
      let* drafts = draft_count (module C) name uid in
      Alcotest.(check int) (name ^ ": no draft") 0 drafts;
      Lwt.return_unit)

let listing_transport_error_case =
  listing_failure_case "listing transport failure: nothing persisted"
    ~username:"ghoauth_ltransport" ~installation:936000015L
    ~responses:[ Error () ] ~expected_listing_calls:1

let listing_status_case =
  listing_failure_case "listing unexpected status: nothing persisted"
    ~username:"ghoauth_lstatus" ~installation:936000016L
    ~responses:[ Ok (401, "{}") ]
    ~expected_listing_calls:1

let listing_invalid_case =
  listing_failure_case "listing invalid response: nothing persisted"
    ~username:"ghoauth_linvalid" ~installation:936000017L
    ~responses:[ Ok (200, "{") ]
    ~expected_listing_calls:1

let listing_no_public_case =
  let installation = 936000018L in
  listing_failure_case "no public repositories: nothing persisted"
    ~username:"ghoauth_lnopublic" ~installation
    ~responses:
      [ repo_listing
          [ repo_entry ~installation ~private_flag:true
              ~visibility:"private"
              ~description:
                (Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
              ~id:611L ~name:Github_fixture.gur_private_name ()
          ]
      ]
    ~expected_listing_calls:1

let listing_pagination_case =
  let installation = 936000019L in
  listing_failure_case "listing pagination limit: nothing persisted"
    ~username:"ghoauth_lpages" ~installation
    ~responses:
      (List.init 20 (fun page ->
           repo_listing ~total:2100
             (List.init 100 (fun i ->
                  let id = Int64.of_int (700000 + (page * 100) + i) in
                  repo_entry ~installation ~id
                    ~name:(Printf.sprintf "repo-%Ld" id)
                    ()))))
    ~expected_listing_calls:20

let multiple_repositories_case =
  db_case "multiple public repositories: exact ordered snapshot"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_multi" in
      let installation = 936000014L in
      let* state, cookie = onboard ~url ~uid ~installation "multi" in
      (* An organization listing mixing UTF-8 and null descriptions, a
         slash-separated default branch, an archived repository, and a
         private entry that must vanish without disturbing positions. *)
      let entries =
        [ repo_entry ~installation ~id:601L ~name:"core"
            ~description:{|"Core — servizio principale"|} ()
        ; repo_entry ~installation ~id:602L ~name:"ops"
            ~default_branch:"release/v1" ()
        ; repo_entry ~installation ~private_flag:true
            ~visibility:"private"
            ~description:(Printf.sprintf {|"%s"|} Github_fixture.gur_private_description)
            ~id:603L ~name:Github_fixture.gur_private_name ()
        ; repo_entry ~installation ~id:604L ~name:"archive" ~archived:true
            ()
        ; repo_entry ~installation ~id:605L ~name:"docs" ()
        ]
      in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for installation ] (ref 0))
          ~repositories:(Github_fixture.gur_transport [ repo_listing entries ] repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_success_lwt "multi" response in
      let* record_id =
        installation_record_id (module C) "multi" installation
      in
      let* draft = active_draft (module C) "multi" ~uid ~record_id in
      let draft = require_draft "multi" draft in
      let* sigs = snapshot_sigs (module C) "multi" draft in
      let account_id = Int64.add installation 1L in
      let login = repo_owner_login installation in
      Alcotest.(check (list string)) "exact contiguous ordered snapshot"
        [ Project_fixture.sig_of ~position:1 ~id:601L ~account_id ~login
            ~description:"Core — servizio principale" "core"
        ; Project_fixture.sig_of ~position:2 ~id:602L ~account_id ~login
            ~branch:"release/v1" "ops"
        ; Project_fixture.sig_of ~position:3 ~id:604L ~account_id ~login
            ~archived:true "archive"
        ; Project_fixture.sig_of ~position:4 ~id:605L ~account_id ~login "docs"
        ]
        sigs;
      Lwt.return_unit)

let draft_failure_case =
  db_case
    "draft failure after installation success: intentional partial \
     persistence, then idempotent retry"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_draftfail" in
      let installation = 936000020L in
      let account_id = Int64.add installation 1L in
      let login = repo_owner_login installation in
      (* Flow 0: a normal success establishes the draft and snapshot the
         later failure must leave untouched. *)
      let* state0, cookie0 = onboard ~url ~uid ~installation "flow 0" in
      let first_sigs =
        [ Project_fixture.sig_of ~position:1 ~id:801L ~account_id ~login "first"
        ]
      in
      let* response =
        run_callback ~url ~jar:[ cookie0 ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for installation ] (ref 0))
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ repo_entry ~installation ~id:801L ~name:"first" () ]
               ]
               (ref []))
          ~target:(callback_target state0 fixture_code)
          ()
      in
      let* () = check_success_lwt "flow 0" response in
      let* record_id =
        installation_record_id (module C) "flow 0" installation
      in
      let* draft0 = active_draft (module C) "flow 0" ~uid ~record_id in
      let draft0 = require_draft "flow 0" draft0 in
      (* Flow 1: the poison repository id makes the snapshot insert fail
         inside the draft store's transaction, deterministically, after
         record_verified has already committed. *)
      let* r = C.exec q_create_fail_fn () in
      let* () = or_fail "create fail fn" r in
      let* r = C.exec q_create_fail_trigger () in
      let* () = or_fail "create fail trigger" r in
      let* state1, cookie1 = onboard ~url ~uid ~installation "flow 1" in
      let* response =
        run_callback ~url ~jar:[ cookie1 ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for installation ] (ref 0))
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ repo_entry ~installation ~id:802L ~name:"second" ()
                   ; repo_entry ~installation ~id:poison_repo_id
                       ~name:"poisoned" ()
                   ]
               ]
               (ref []))
          ~target:(callback_target state1 fixture_code)
          ()
      in
      let* () = check_failure_lwt "flow 1" response in
      check_deletion "flow 1" ~cookie_name:(fst cookie1)
        ~stored_value:(snd cookie1) response;
      let* _, consumed1 = state_row (module C) "flow 1" state1 in
      Alcotest.(check bool) "flow 1 state consumed" false consumed1;
      (* Intentional partial persistence: the installation row survives
         active — no compensating deletion — while the previous draft and
         snapshot are untouched by the rolled-back refresh. *)
      let* count = installation_count (module C) "flow 1" installation in
      Alcotest.(check int) "one installation row" 1 count;
      let* status = C.find q_installation_status installation in
      let* status = or_fail "installation status" status in
      Alcotest.(check string) "installation still active" "active" status;
      let* draft1 = active_draft (module C) "flow 1" ~uid ~record_id in
      Alcotest.(check bool) "previous draft still the active one" true
        (draft1 = Some draft0);
      let* drafts = draft_count (module C) "flow 1" uid in
      Alcotest.(check int) "no extra draft" 1 drafts;
      let* sigs = snapshot_sigs (module C) "flow 1" draft0 in
      Alcotest.(check (list string)) "previous snapshot unchanged"
        first_sigs sigs;
      (* Remove the injected failure; a completely fresh flow reuses the
         installation idempotently and refreshes the draft with a
         complete new snapshot. *)
      let* r = C.exec q_drop_fail_trigger () in
      let* () = or_fail "drop trigger" r in
      let* r = C.exec q_drop_fail_fn () in
      let* () = or_fail "drop fn" r in
      let* state2, cookie2 = onboard ~url ~uid ~installation "flow 2" in
      let* response =
        run_callback ~url ~jar:[ cookie2 ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for installation ] (ref 0))
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ repo_entry ~installation ~id:803L ~name:"third" ()
                   ; repo_entry ~installation ~id:804L ~name:"fourth" ()
                   ]
               ]
               (ref []))
          ~target:(callback_target state2 fixture_code)
          ()
      in
      let* () = check_success_lwt "flow 2" response in
      let* count = installation_count (module C) "flow 2" installation in
      Alcotest.(check int) "still exactly one installation row" 1 count;
      let* active = active_draft_count (module C) "flow 2" uid in
      Alcotest.(check int) "exactly one active draft" 1 active;
      let* draft2 = active_draft (module C) "flow 2" ~uid ~record_id in
      let draft2 = require_draft "flow 2" draft2 in
      let* sigs = snapshot_sigs (module C) "flow 2" draft2 in
      Alcotest.(check (list string)) "complete retry snapshot"
        [ Project_fixture.sig_of ~position:1 ~id:803L ~account_id ~login "third"
        ; Project_fixture.sig_of ~position:2 ~id:804L ~account_id ~login
            "fourth"
        ]
        sigs;
      Lwt.return_unit)

(* --- Analytics: github_app_installed ---

   The success boundary is exactly "state consumed, installation verified,
   and BOTH persistence steps committed". The distinct id comes from the
   consumed state row's owning user — this leg is sessionless, so a session
   could not supply it. The fake sink replaces the HTTP transport, so no
   PostHog request can leave the process. *)

let analytics_success_case =
  db_case
    "analytics: a completed callback captures exactly one \
     github_app_installed for the state's owner, carrying no GitHub or \
     OAuth material"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_an_ok" in
      let installation = 936000101L in
      let* state, cookie = onboard ~url ~uid ~installation "analytics" in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response, captured =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_callback ~url ~jar:[ cookie; Analytics_fixture.an_consent_granted ]
              ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
              ~installations:
                (installations_stub [ listing_for installation ] inst_calls)
              ~repositories:
                (Github_fixture.gur_transport
                   [ repo_listing (default_repo_entries installation) ]
                   repo_captured)
              ~target:(callback_target state fixture_code)
              ())
      in
      let* () = check_success_lwt "analytics success" response in
      Analytics_fixture.check_single_capture "granted" ~name:"github_app_installed"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      (* Privacy sweep over the serialized payload: none of the
         credential-shaped fixtures this leg handles may appear anywhere
         in it. Assertions are boolean so a failure never prints the
         payload — or the values it is being checked against. *)
      let serialized =
        String.concat "|" (List.map Yojson.Safe.to_string captured)
      in
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            "credential-shaped fixture absent from the analytics payload"
            false
            (Html_assert.contains_nonempty ~needle serialized))
        [ access_fixture; refresh_fixture; fixture_code; Github_fixture.gte_client_secret;
          GOC.state_to_string state;
          Int64.to_string installation;
          Int64.to_string (Int64.add installation 1L);
          Printf.sprintf "owner-%Ld" installation;
          Github_fixture.gur_private_name; Github_fixture.gur_private_description; snd cookie
        ];
      Lwt.return_unit)

let analytics_consent_case =
  db_case
    "analytics: denied, missing, and disabled configurations capture \
     nothing while the callback still succeeds"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let run label ?(enabled = true) ~username ~installation extra_cookies =
        let* uid = fixture_user (module C) username in
        let* state, cookie = onboard ~url ~uid ~installation label in
        let ex_calls = ref 0 and inst_calls = ref 0 in
        let repo_captured = ref [] in
        let* response, captured =
          Analytics_fixture.with_sink_lwt ~enabled (fun () ->
              run_callback ~url ~jar:(cookie :: extra_cookies)
                ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
                ~installations:
                  (installations_stub [ listing_for installation ] inst_calls)
                ~repositories:
                  (Github_fixture.gur_transport
                     [ repo_listing (default_repo_entries installation) ]
                     repo_captured)
                ~target:(callback_target state fixture_code)
                ())
        in
        (* The business outcome is identical in every configuration. *)
        let* () = check_success_lwt label response in
        Analytics_fixture.check_no_capture label captured;
        Lwt.return_unit
      in
      let* () =
        run "denied" ~username:"ghoauth_an_denied" ~installation:936000102L
          [ Analytics_fixture.an_consent_denied ]
      in
      let* () =
        run "missing" ~username:"ghoauth_an_missing"
          ~installation:936000103L []
      in
      run "analytics disabled" ~enabled:false
        ~username:"ghoauth_an_disabled" ~installation:936000104L
        [ Analytics_fixture.an_consent_granted ])

let analytics_no_event_case =
  db_case
    "analytics: a rejected authorization, a failed exchange, a failed \
     verification, a failed listing, a failed persistence, and a replay \
     all capture nothing"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      (* One helper per terminal-failure shape, each over the real
         pipeline with granted consent. *)
      let failing label ~username ~installation ~target ~exchange
          ~installations ~repositories =
        let* uid = fixture_user (module C) username in
        let* state, cookie = onboard ~url ~uid ~installation label in
        let* response, captured =
          Analytics_fixture.with_sink_lwt (fun () ->
              run_callback ~url ~jar:[ cookie; Analytics_fixture.an_consent_granted ]
                ~exchange ~installations ~repositories
                ~target:(target state) ())
        in
        let* () = check_failure_lwt label response in
        Analytics_fixture.check_no_capture label captured;
        Lwt.return (uid, state, cookie)
      in
      let ex_calls = ref 0 and inst_calls = ref 0 in
      let repo_captured = ref [] in
      (* The user said no at GitHub: no exchange, no SQL past consume. *)
      let* _ =
        failing "authorization rejected" ~username:"ghoauth_an_rej"
          ~installation:936000111L
          ~target:(fun state ->
            target_of
              [ "state=" ^ GOC.state_to_string state; "error=access_denied" ])
          ~exchange:(exchange_stub (Ok (200, token_body)) ex_calls)
          ~installations:(installations_stub [] inst_calls)
          ~repositories:(Github_fixture.gur_transport [] repo_captured)
      in
      Alcotest.(check int) "rejected: no exchange" 0 !ex_calls;
      (* Exchange failure. *)
      let* _ =
        failing "exchange failure" ~username:"ghoauth_an_ex"
          ~installation:936000112L
          ~target:(fun state -> callback_target state fixture_code)
          ~exchange:(exchange_stub (Error ()) (ref 0))
          ~installations:(installations_stub [] (ref 0))
          ~repositories:(Github_fixture.gur_transport [] (ref []))
      in
      (* Verification failure: an empty installation listing. *)
      let* _ =
        failing "verification failure" ~username:"ghoauth_an_ver"
          ~installation:936000113L
          ~target:(fun state -> callback_target state fixture_code)
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:(installations_stub [ listing_for 936000999L ] (ref 0))
          ~repositories:(Github_fixture.gur_transport [] (ref []))
      in
      (* Repository listing failure: the installation is verified, but
         nothing is persisted, so there is no installation to report. *)
      let* _ =
        failing "listing failure" ~username:"ghoauth_an_list"
          ~installation:936000114L
          ~target:(fun state -> callback_target state fixture_code)
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub [ listing_for 936000114L ] (ref 0))
          ~repositories:(Github_fixture.gur_transport [ Error () ] (ref []))
      in
      (* A successful callback, then its replay. *)
      let* uid = fixture_user (module C) "ghoauth_an_replay" in
      let installation = 936000115L in
      let* state, cookie = onboard ~url ~uid ~installation "replay" in
      let succeed label jar =
        Analytics_fixture.with_sink_lwt (fun () ->
            run_callback ~url ~jar
              ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
              ~installations:
                (installations_stub [ listing_for installation ] (ref 0))
              ~repositories:
                (Github_fixture.gur_transport
                   [ repo_listing (default_repo_entries installation) ]
                   (ref []))
              ~target:(callback_target state fixture_code)
              ())
        |> Lwt.map (fun (response, captured) -> (label, response, captured))
      in
      let* label, response, captured =
        succeed "first" [ cookie; Analytics_fixture.an_consent_granted ]
      in
      let* () = check_success_lwt label response in
      Analytics_fixture.check_single_capture label ~name:"github_app_installed"
        ~distinct_id:(Printf.sprintf "user:%d" uid)
        ~props:[ ("user_id", `Int uid) ]
        captured;
      (* The replay finds the state already consumed: same generic
         failure, and deliberately no second event. *)
      let* label, response, captured =
        succeed "replay" [ cookie; Analytics_fixture.an_consent_granted ]
      in
      let* () = check_failure_lwt label response in
      Analytics_fixture.check_no_capture label captured;
      Lwt.return_unit)

(* --- Both GitHub account kinds, end to end ---
   The fixtures above derive one synthetic identity per installation and
   always describe it as an organization. The two cases below instead
   spell out the identity GitHub really returns for each account kind —
   a personal account and an organization with its own login and account
   id — and follow it all the way to what /projects/new renders, because
   that is where a wrongly parsed or wrongly persisted account kind would
   actually surface. *)

(* One listing page carrying an explicit account identity and target
   type, instead of the id-derived default. *)
let listing_as ~account_id ~login ~target installation =
  Ok
    ( 200,
      Github_fixture.gui_entry_page
        (Github_fixture.gui_raw_entry
           ~id:(Int64.to_string installation)
           ~account:
             (Printf.sprintf {|{"id":%Ld,"login":"%s"}|} account_id login)
           ~target:(Printf.sprintf {|"%s"|} target)
           ()) )

(* GET /projects/new exactly as the success redirect issues it —
   parameter-free, with a memory session for the owner. With one owned
   draft the handler must open that draft's repository selector directly,
   so this is the real page a user lands on after a successful callback. *)
let projects_new_body ~url ~uid label =
  let handler =
    Dream.memory_sessions (fun req ->
        let* () =
          Dream.set_session_field req "user_id" (string_of_int uid)
        in
        Earde.Project_setup_handlers.make_new_project_handler
          ~mode:Ob.Public req)
  in
  let* response =
    run_shared ~url handler
      (Dream.request ~method_:`GET ~target:"/projects/new" "")
  in
  Alcotest.(check int) (label ^ ": /projects/new renders") 200
    (status_of response);
  Dream.body response

(* One account kind, from the GitHub identity through persistence to the
   rendered chooser. [private_name] must never appear anywhere. *)
let account_kind_case name ~username ~installation ~account_id ~login
    ~target ~expected_type ~public_name ~private_name =
  db_case name (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) username in
      let* state, cookie = onboard ~url ~uid ~installation name in
      let repo ?(private_flag = false) ?(visibility = "public") ~id
          repo_name =
        Github_fixture.gur_repo ~owner_id:account_id ~owner_login:login ~private_flag
          ~visibility ~id ~name:repo_name ()
      in
      let inst_calls = ref 0 in
      let repo_captured = ref [] in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub
               [ listing_as ~account_id ~login ~target installation ]
               inst_calls)
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ repo ~id:901L public_name
                   ; repo ~private_flag:true ~visibility:"private"
                       ~id:902L private_name
                   ] ]
               repo_captured)
          ~target:(callback_target state fixture_code)
          ()
      in
      (* The whole point: this account kind reaches the connected
         redirect, not the generic failure. *)
      let* () = check_success_lwt name response in
      Alcotest.(check int) (name ^ ": one verification") 1 !inst_calls;
      Alcotest.(check int) (name ^ ": one repository listing") 1
        (List.length !repo_captured);
      (* The account kind and login are persisted exactly as GitHub
         described them — no defaulting to the other kind. *)
      let* row = C.find q_installation_row installation in
      let* (stored_account_id, stored_login, stored_type), connected_by =
        or_fail (name ^ ": installation row") row
      in
      Alcotest.(check bool) (name ^ ": account id") true
        (stored_account_id = account_id);
      Alcotest.(check string) (name ^ ": account login") login stored_login;
      Alcotest.(check string) (name ^ ": account type") expected_type
        stored_type;
      Alcotest.(check (option int)) (name ^ ": connected by the owner")
        (Some uid) connected_by;
      (* Exactly the public repository is snapshotted, owned by this
         account. *)
      let* record_id = installation_record_id (module C) name installation in
      let* draft = active_draft (module C) name ~uid ~record_id in
      let draft = require_draft name draft in
      let* sigs = snapshot_sigs (module C) name draft in
      Alcotest.(check (list string)) (name ^ ": public snapshot only")
        [ Project_fixture.sig_of ~position:1 ~id:901L ~account_id ~login
            public_name ]
        sigs;
      (* And following the success redirect itself — parameter-free —
         opens this single draft's repository selector, with the private
         repository absent from the page as well as from the snapshot. *)
      let* html = projects_new_body ~url ~uid name in
      Alcotest.(check bool) (name ^ ": public repository offered") true
        (Html_assert.contains_nonempty ~needle:(login ^ "/" ^ public_name) html);
      Alcotest.(check bool) (name ^ ": account login shown") true
        (Html_assert.contains_nonempty ~needle:login html);
      Alcotest.(check bool) (name ^ ": private repository absent") false
        (Html_assert.contains_nonempty ~needle:private_name html);
      Lwt.return_unit)

let personal_account_case =
  account_kind_case
    "personal account: User installation connects and lands on \
     /projects/new"
    ~username:"ghoauth_personal" ~installation:936000021L
    ~account_id:220767424L ~login:"personal-owner-fixture" ~target:"User"
    ~expected_type:"user" ~public_name:"smoke-personal"
    ~private_name:"personal-private-fixture"

let organization_account_case =
  account_kind_case
    "organization: Organization installation with an org-owned public \
     repository connects and lands on /projects/new"
    ~username:"ghoauth_org" ~installation:936000022L
    ~account_id:267672683L ~login:"org-owner-fixture"
    ~target:"Organization" ~expected_type:"organization"
    ~public_name:"smoke-org" ~private_name:"org-private-fixture"

(* An organization listing whose repository is owned by a different
   account than the installation: the ownership check must reject the
   page rather than snapshot a foreign repository. *)
let organization_owner_mismatch_case =
  db_case
    "organization owner mismatch: foreign repository fails closed with \
     no rows"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_orgowner" in
      let installation = 936000023L in
      let account_id = 267672683L in
      let* state, cookie = onboard ~url ~uid ~installation "org owner" in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub
               [ listing_as ~account_id ~login:"org-owner-fixture"
                   ~target:"Organization" installation ]
               (ref 0))
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ Github_fixture.gur_repo
                       ~owner_id:(Int64.add account_id 7L)
                       ~owner_login:"someone-else-fixture" ~id:903L
                       ~name:"borrowed" () ] ]
               (ref []))
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () =
        check_terminal_failure (module C) "org owner" ~cookie ~installation
          response
      in
      let* drafts = draft_count (module C) "org owner" uid in
      Alcotest.(check int) "no draft" 0 drafts;
      Lwt.return_unit)

(* The same installation id already recorded as a personal account: an
   organization verification for it is an identity change the store must
   refuse, leaving the existing row exactly as it was. *)
let account_kind_conflict_case =
  db_case
    "account-kind conflict: an organization verification never rewrites \
     a recorded personal installation"
    (fun ~url (module C : Caqti_lwt.CONNECTION) ->
      let* uid = fixture_user (module C) "ghoauth_kindconflict" in
      let installation = 936000024L in
      let account_id = 220767424L in
      let* r = C.exec q_insert_active_user (installation, account_id) in
      let* () = or_fail "personal fixture" r in
      let* state, cookie = onboard ~url ~uid ~installation "conflict" in
      let* response =
        run_callback ~url ~jar:[ cookie ]
          ~exchange:(exchange_stub (Ok (200, token_body)) (ref 0))
          ~installations:
            (installations_stub
               [ listing_as ~account_id ~login:"org-owner-fixture"
                   ~target:"Organization" installation ]
               (ref 0))
          ~repositories:
            (Github_fixture.gur_transport
               [ repo_listing
                   [ Github_fixture.gur_repo ~owner_id:account_id
                       ~owner_login:"org-owner-fixture" ~id:904L
                       ~name:"conflicting" () ] ]
               (ref []))
          ~target:(callback_target state fixture_code)
          ()
      in
      let* () = check_failure_lwt "conflict" response in
      check_deletion "conflict" ~cookie_name:(fst cookie)
        ~stored_value:(snd cookie) response;
      (* The recorded personal identity survives byte for byte, and no
         draft was created against it. *)
      let* row = C.find q_installation_row installation in
      let* (stored_account_id, stored_login, stored_type), _ =
        or_fail "conflict row" row
      in
      Alcotest.(check bool) "account id unchanged" true
        (stored_account_id = account_id);
      Alcotest.(check string) "login unchanged" "personal-owner-fixture"
        stored_login;
      Alcotest.(check string) "kind unchanged" "user" stored_type;
      let* count = installation_count (module C) "conflict" installation in
      Alcotest.(check int) "still exactly one row" 1 count;
      let* drafts = draft_count (module C) "conflict" uid in
      Alcotest.(check int) "no draft" 0 drafts;
      Lwt.return_unit)

let db_suite =
  [ success_case; replay_case; expired_case; already_consumed_case;
    personal_account_case; organization_account_case;
    organization_owner_mismatch_case; account_kind_conflict_case;
    binding_mismatch_case; exchange_transport_case; oauth_rejected_case;
    not_accessible_case; pagination_limit_case;
    listing_transport_error_case; listing_status_case;
    listing_invalid_case; listing_no_public_case; listing_pagination_case;
    multiple_repositories_case; persistence_failure_case;
    draft_failure_case; consume_storage_error_case; parallel_case;
    analytics_success_case; analytics_consent_case; analytics_no_event_case ]

let suites =
    (* OAuth-callback handler gates: DB-free with injected
       mode/config/credentials, real encrypted cookies, and fake
       transports — every outcome must be a clean 303 to one of the two
       generic targets, with no stage distinguishable. *)
  [ ( "github_oauth_callback_gates", gate_suite )
    (* OAuth callback over the real pipeline (start + setup return first,
       then sql_pool + secret with fake transports, no session
       middleware); EARDE_TEST_DATABASE_URL gate. *)
  ; ( "github_oauth_callback_db", db_suite )
  ]
