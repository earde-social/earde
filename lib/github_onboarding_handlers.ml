(* HTTP layer for starting the GitHub App installation flow, kept out of the
   legacy Handlers macro-module per the feature-module guideline. The handler
   is a factory over the closed onboarding mode and a config loader so tests
   can inject fixed values without touching the process environment, disabled
   or rejected requests never trigger configuration reads, and configuration
   failures stay controlled. *)

(* Session read that also behaves in tests where no session middleware is
   installed: no middleware simply means no authenticated session. *)
let session_field_opt request name =
  match Dream.session_field request name with
  | exception _ -> None
  | value -> value

let session_user request = session_field_opt request "username"

(* The authenticated user, straight from the authoritative Dream session —
   never from caller-supplied input. Anything absent, non-numeric, zero, or
   negative reads as "not logged in". *)
let authenticated_user_id request =
  match session_field_opt request "user_id" with
  | None -> None
  | Some raw -> (
      match int_of_string_opt raw with
      | Some id when id > 0 -> Some id
      | _ -> None)

let not_found_page request =
  Dream.respond ~status:`Not_Found
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This integration is not available." ~alert_type:"error"
       ~return_url:"/" request)

let forbidden_page request =
  Dream.respond ~status:`Forbidden
    (Pages.msg_page ?user:(session_user request) ~title:"Not Allowed"
       ~message:"This request is not allowed." ~alert_type:"error"
       ~return_url:"/bring" request)

(* One generic 503 for every failure past the gates — configuration, state
   issuance, cookie storage. No environment-variable names, configuration
   values, URLs, or error constructors ever reach the page. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable
    (Pages.msg_page ?user:(session_user request) ~title:"Temporarily Unavailable"
       ~message:
         "GitHub connection is temporarily unavailable. Please try again \
          later."
       ~alert_type:"error" ~return_url:"/bring" request)

(* The effective (scheme, host, port) identity of a serialized origin, or
   [None] for anything malformed. An Origin header is scheme://host[:port]
   only, so a path, query, fragment, or userinfo component (and non-http(s)
   schemes, including "null") reads as malformed rather than being repaired. *)
let origin_key value =
  let value = String.trim value in
  if String.equal value "" then None
  else
    let uri = Uri.of_string value in
    match (Uri.scheme uri, Uri.host uri) with
    | Some scheme, Some host when not (String.equal host "") -> (
        let scheme = String.lowercase_ascii scheme in
        match scheme with
        | "http" | "https" ->
            if
              (not (String.equal (Uri.path uri) ""))
              || Uri.query uri <> []
              || Uri.fragment uri <> None
              || Uri.userinfo uri <> None
            then None
            else
              let default = if String.equal scheme "https" then 443 else 80 in
              Some
                ( scheme,
                  String.lowercase_ascii host,
                  Option.value (Uri.port uri) ~default )
        | _ -> None)
    | _ -> None

(* Same-origin browser check for this state-changing POST. A present Origin
   decides alone — it must match the validated public origin exactly (by
   normalized scheme, host, and effective port), and a malformed or
   mismatching value is rejected even if Sec-Fetch-Site claims same-origin.
   Only when Origin is absent does exactly "same-origin" fetch metadata
   suffice; "same-site", "cross-site", and "none" do not. Host, Referer, and
   forwarding headers are deliberately never consulted. *)
let same_origin_request config request =
  match Dream.header request "Origin" with
  | Some origin -> (
      match
        ( origin_key origin,
          origin_key (Github_app_config.public_origin config) )
      with
      | Some supplied, Some configured -> supplied = configured
      | _ -> false)
  | None -> (
      match Option.map String.trim (Dream.header request "Sec-Fetch-Site") with
      | Some "same-origin" -> true
      | _ -> false)

(* --- GitHub App setup return (GET /integrations/github/install/return) --- *)

(* Every setup-return outcome must move the browser away from the callback
   URL: no branch may render HTML while state sits in the address bar, the
   redirect must not be cached, and Referrer-Policy: no-referrer keeps the
   state-bearing URL from reaching the next destination as a Referer — both
   for local /bring failures and for the external GitHub redirect. *)
let clean_redirect location =
  Dream.response ~status:`See_Other
    ~headers:
      [ ("Location", location);
        ("Cache-Control", "no-store");
        ("Pragma", "no-cache");
        ("Referrer-Policy", "no-referrer");
      ]
    ""

let bring_redirect () = clean_redirect "/bring"

(* Every occurrence of one raw query key in the original request target, in
   order: [Some value] for [key=value] (the value byte-for-byte after the
   first '='), [None] for a bare [key]. Dream.query silently tolerates
   duplicate keys, so the target is split by hand: only the substring after
   the first '?', stopped at a raw '#', components on raw '&'. Keys are
   case-sensitive; unrelated parameters are ignored (GitHub may append
   optional metadata). Values are never percent-decoded — the canonical
   state alphabet needs none, and decoding would create aliases of one
   stored value. Distinguishing an absent key ([]) from a malformed one
   lets the OAuth callback enforce code-XOR-error strictly. *)
let raw_query_occurrences ~key target =
  match String.index_opt target '?' with
  | None -> []
  | Some q ->
      let query = String.sub target (q + 1) (String.length target - q - 1) in
      let query =
        match String.index_opt query '#' with
        | None -> query
        | Some h -> String.sub query 0 h
      in
      List.filter_map
        (fun component ->
          match String.index_opt component '=' with
          | None ->
              (* A bare required key carries no usable value and must fail
                 the exactly-one rule; anything else is unrelated. *)
              if String.equal component key then Some None else None
          | Some eq ->
              if String.equal (String.sub component 0 eq) key then
                Some
                  (Some
                     (String.sub component (eq + 1)
                        (String.length component - eq - 1)))
              else None)
        (String.split_on_char '&' query)

(* Strict single-occurrence extraction: exactly one non-empty value. *)
let raw_query_value ~key target =
  match raw_query_occurrences ~key target with
  | [ Some value ] when not (String.equal value "") -> Ok value
  | _ -> Error ()

(* GitHub echoes installation_id as an untrusted decimal BIGINT: accept
   ASCII digits only, require the value to fit int64 and be positive.
   Leading zeroes are tolerated — they cannot alias a different id — but
   signs, whitespace, decimal points, and overflow are rejected. *)
let installation_id_of_string value =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  if String.equal value "" || not (String.for_all is_digit value) then None
  else
    let rec accumulate i acc =
      if i = String.length value then Some acc
      else
        let d = Int64.of_int (Char.code value.[i] - Char.code '0') in
        if Int64.compare acc (Int64.div (Int64.sub Int64.max_int d) 10L) > 0
        then None
        else accumulate (i + 1) (Int64.add (Int64.mul acc 10L) d)
    in
    match accumulate 0 0L with
    | Some id when Int64.compare id 0L > 0 -> Some id
    | _ -> None

(* Both required parameters, or one payload-free rejection — attacker input
   never surfaces in a message or log. *)
let parse_setup_return_target target =
  match raw_query_value ~key:"state" target with
  | Error () -> Error ()
  | Ok raw_state -> (
      match Github_onboarding_crypto.state_of_callback raw_state with
      | Error Github_onboarding_crypto.Invalid_format -> Error ()
      | Ok state -> (
          match raw_query_value ~key:"installation_id" target with
          | Error () -> Error ()
          | Ok raw_id -> (
              match installation_id_of_string raw_id with
              | None -> Error ()
              | Some installation_id -> Ok (state, installation_id))))

let make_setup_return_handler ~mode ~load_config request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no configuration read, no parsing, no cookie access,
         no SQL. Still a clean redirect rather than a 404 — the browser is
         mid-flow at a URL carrying a live state. *)
      Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      (* Deliberately no Dream-session gate: this arrives on a cross-site
         top-level redirect from GitHub that the login session cookie is not
         guaranteed to accompany. Authorization is possession-based — the
         raw state plus the per-flow cookie — and the owning user was
         recorded by the authenticated start endpoint. *)
      match load_config () with
      | Error _ -> Lwt.return (bring_redirect ())
      | Ok config -> (
          match parse_setup_return_target (Dream.target request) with
          | Error () -> Lwt.return (bring_redirect ())
          | Ok (state, installation_id) -> (
              match Github_onboarding_cookie.load config ~request ~state with
              | Error Github_onboarding_cookie.Missing ->
                  (* Another browser, or cleared data: leave the state and
                     any other browser's cookie intact so the original
                     browser can still finish. No SQL was touched. *)
                  Lwt.return (bring_redirect ())
              | Error Github_onboarding_cookie.Invalid ->
                  (* Undecryptable or undecodable material is useless;
                     delete it. Whether decryption or decoding failed stays
                     private. *)
                  let response = bring_redirect () in
                  Github_onboarding_cookie.drop config ~request ~response
                    ~state;
                  Lwt.return response
              | Ok data -> (
                  let%lwt attached =
                    Dream.sql request (fun db ->
                        Github_onboarding_state_store
                        .attach_pending_installation db ~state
                          ~session_binding_hash:
                            (Github_onboarding_session_data
                             .session_binding_hash data)
                          ~flow:Github_onboarding.Project_onboarding
                          ~pending_github_installation_id:installation_id)
                  in
                  match attached with
                  | Ok () ->
                      (* The per-flow cookie is left untouched — not
                         refreshed, not re-emitted: the OAuth callback still
                         needs its binding proof and PKCE verifier under the
                         original lifetime. *)
                      Lwt.return
                        (clean_redirect
                           (Github_onboarding_urls.authorization_url config
                              ~state
                              ~code_challenge:
                                (Github_onboarding_session_data.code_challenge
                                   data)))
                  | Error
                      ( Github_onboarding_state_store.State_unavailable
                      | Github_onboarding_state_store
                        .Invalid_pending_installation_id ) ->
                      (* One collapsed answer for every dead-state cause —
                         unknown, expired, consumed, mismatched, or a
                         conflicting installation — so the endpoint is not a
                         state-probing oracle. The cookie cannot succeed
                         against this row again, so drop it. *)
                      let response = bring_redirect () in
                      Github_onboarding_cookie.drop config ~request ~response
                        ~state;
                      Lwt.return response
                  | Error Github_onboarding_state_store.Storage_error ->
                      (* Transient database failure: keep the cookie so a
                         browser refresh can retry while the state is still
                         live. *)
                      Lwt.return (bring_redirect ())))))

let make_start_installation_handler ~mode ~load_config request =
  match mode with
  (* Off: controlled 404 before configuration, session material, or SQL. *)
  | Project_onboarding.Off -> not_found_page request
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Dream.redirect request "/login"
      | Some user_id ->
          let is_admin =
            session_field_opt request "is_admin" = Some "true"
          in
          if not (Project_onboarding.onboarding_available mode ~is_admin) then
            forbidden_page request
          else (
            match load_config () with
            | Error _ -> unavailable_page request
            | Ok config ->
                if not (same_origin_request config request) then
                  forbidden_page request
                else
                  (* All gates passed: only now generate random material and
                     touch the database. Only the binding hash leaves the
                     per-flow data; the raw binding and verifier live solely
                     in the encrypted cookie. *)
                  let data = Github_onboarding_session_data.create () in
                  let%lwt issued =
                    Dream.sql request (fun db ->
                        Github_onboarding_state_store.issue db ~user_id
                          ~session_binding_hash:
                            (Github_onboarding_session_data
                             .session_binding_hash data)
                          ~flow:Github_onboarding.Project_onboarding)
                  in
                  (match issued with
                  | Error _ -> unavailable_page request
                  | Ok state -> (
                      let location =
                        Github_onboarding_urls.installation_url config ~state
                      in
                      (* Explicit 303 with a verbatim Location — the request
                         is a POST, so nothing may depend on a default 302,
                         and the URL must reach the browser exactly as built.
                         Cookie encryption can raise if the runtime secret
                         middleware is misconfigured; in that case the
                         already-inserted state row is left to expire (15
                         minutes) and the browser gets the same generic 503 —
                         never a GitHub redirect it could not complete. The
                         handler's only exception guard sits here, around
                         response and cookie construction. *)
                      match
                        try
                          let response =
                            Dream.response ~status:`See_Other
                              ~headers:[ ("Location", location) ]
                              ""
                          in
                          Github_onboarding_cookie.store config ~request
                            ~response ~state data;
                          Some response
                        with _ -> None
                      with
                      | Some response -> Lwt.return response
                      | None -> unavailable_page request))))

(* --- OAuth authorization callback
   (GET /integrations/github/authorize/callback) --- *)

(* The whole callback has exactly two clean local outcomes. One shared
   failure target for every internal stage — malformed callback, GitHub
   rejection, cookie, configuration, credentials, consumption, exchange,
   verification, persistence — so the browser can never learn which stage
   failed, and no sensitive value rides in either Location. *)
let callback_failure () = clean_redirect "/bring?github=failed"
let callback_success () = clean_redirect "/bring?github=connected"

(* Applies this flow's cookie deletion to an already-built clean redirect.
   Dropping can only raise if the runtime secret middleware is
   misconfigured; the clean redirect still wins over surfacing that
   exception. Cookie material is never inspected or logged. *)
let redirect_dropping_cookie config ~request ~state response =
  (try Github_onboarding_cookie.drop config ~request ~response ~state
   with _ -> ());
  response

let callback_failure_dropping config ~request ~state =
  redirect_dropping_cookie config ~request ~state (callback_failure ())

(* The two accepted callback shapes: GitHub sent an authorization code, or
   GitHub reported the authorization as rejected. The remote error value is
   deliberately not carried — never inspected, classified, decoded, or
   logged. *)
type oauth_callback_shape =
  | Authorization_granted of Github_oauth_token_exchange.authorization_code
  | Authorization_rejected

(* Exactly one non-empty canonical [state], then code XOR error: a single
   non-empty valid [code] with no canonical [error] parameter, or a single
   non-empty [error] with no canonical [code] parameter. Duplicates,
   blanks, bare keys, both-present, and neither-present are all one
   payload-free rejection; unrelated keys (error_description, error_uri,
   uppercase lookalikes) never reach the occurrence scan's key match. *)
let parse_oauth_callback_target target =
  match raw_query_value ~key:"state" target with
  | Error () -> Error ()
  | Ok raw_state -> (
      match Github_onboarding_crypto.state_of_callback raw_state with
      | Error Github_onboarding_crypto.Invalid_format -> Error ()
      | Ok state -> (
          match
            ( raw_query_occurrences ~key:"code" target,
              raw_query_occurrences ~key:"error" target )
          with
          | [ Some raw_code ], [] when not (String.equal raw_code "") -> (
              match
                Github_oauth_token_exchange.authorization_code_of_callback
                  raw_code
              with
              | Ok code -> Ok (state, Authorization_granted code)
              | Error Github_oauth_token_exchange.Invalid_code -> Error ())
          | [], [ Some raw_error ] when not (String.equal raw_error "") ->
              Ok (state, Authorization_rejected)
          | _ -> Error ()))

(* Consume → exchange → verify → persist, in that order. The two Dream.sql
   scopes are separate and short on purpose: no database-pool connection is
   ever held across an outbound GitHub call. Every branch past a successful
   consumption is terminal — the state is burned or spent either way — so
   each deletes the per-flow cookie on its way out. *)
let finish_authorization ~config ~credentials ~exchange_transport
    ~installations_transport ~request ~state ~data ~code =
  let%lwt consumed =
    Dream.sql request (fun db ->
        Github_onboarding_state_store.consume db ~state
          ~session_binding_hash:
            (Github_onboarding_session_data.session_binding_hash data)
          ~flow:Github_onboarding.Project_onboarding)
  in
  match consumed with
  | Error Github_onboarding_state_store.Storage_error ->
      (* No committed outcome is known, so the cookie survives: if the
         database recovers while the state is still live, a browser
         refresh can retry. No GitHub call is made. *)
      Lwt.return (callback_failure ())
  | Error
      ( Github_onboarding_state_store.State_not_found
      | Github_onboarding_state_store.State_expired
      | Github_onboarding_state_store.State_already_consumed
      | Github_onboarding_state_store.Session_binding_mismatch
      | Github_onboarding_state_store.Flow_mismatch
      | Github_onboarding_state_store.Missing_pending_installation ) ->
      (* One collapsed answer whether the state was already dead or was
         just atomically burned by a mismatch — which one stays private,
         and the cookie can never succeed against this row again. *)
      Lwt.return (callback_failure_dropping config ~request ~state)
  | Ok
      {
        Github_onboarding_state_store.user_id;
        flow = _;
        pending_github_installation_id;
      } -> (
      let%lwt exchanged =
        Github_oauth_token_exchange.exchange ~transport:exchange_transport
          ~config ~credentials ~code
          ~verifier:(Github_onboarding_session_data.verifier data)
      in
      match exchanged with
      | Error
          ( Github_oauth_token_exchange.Transport_error
          | Github_oauth_token_exchange.Unexpected_http_status _
          | Github_oauth_token_exchange.OAuth_rejected
          | Github_oauth_token_exchange.Invalid_response ) ->
          (* The state is already consumed: onboarding must restart. *)
          Lwt.return (callback_failure_dropping config ~request ~state)
      | Ok token_set -> (
          let%lwt verification =
            Github_user_installations.verify
              ~transport:installations_transport ~token_set
              ~installation_id:pending_github_installation_id
          in
          match verification with
          | Error
              ( Github_user_installations.Invalid_installation_id
              | Github_user_installations.Transport_error
              | Github_user_installations.Unexpected_http_status _
              | Github_user_installations.Invalid_response
              | Github_user_installations.Installation_not_accessible
              | Github_user_installations.Pagination_limit ) ->
              Lwt.return (callback_failure_dropping config ~request ~state)
          | Ok verified_installation -> (
              (* The token set stays behind in memory on purpose: only the
                 verified identity crosses into persistence. *)
              let%lwt persisted =
                Dream.sql request (fun db ->
                    Github_installation_store.record_verified db
                      ~connected_by_user_id:user_id verified_installation)
              in
              match persisted with
              | Error
                  ( Github_installation_store.Invalid_connected_by_user_id
                  | Github_installation_store.Installation_unavailable
                  | Github_installation_store.Storage_error ) ->
                  Lwt.return
                    (callback_failure_dropping config ~request ~state)
              | Ok () ->
                  Lwt.return
                    (redirect_dropping_cookie config ~request ~state
                       (callback_success ())))))

(* --- Onboarding entry and return page (GET /bring) --- *)

(* Strict interpretation of the callback's two clean redirect targets.
   Case-sensitive exact values, duplicate-aware via raw_query_occurrences:
   anything but exactly one recognized value — missing, blank, bare,
   unknown, differently-cased, duplicated, or conflicting — means no
   banner, and the raw value never reaches the page or a log (only a closed
   variant crosses). Presentation-only: the parameter never changes which
   action the page offers. *)
let bring_feedback_of_target target =
  match raw_query_occurrences ~key:"github" target with
  | [ Some "connected" ] -> Some Github_onboarding_pages.Connected
  | [ Some "failed" ] -> Some Github_onboarding_pages.Failed
  | _ -> None

let make_bring_handler ~mode request =
  let user_id = authenticated_user_id request in
  let access =
    match (mode : Project_onboarding.mode) with
    | Off -> Github_onboarding_pages.Onboarding_disabled
    | Admins | Public -> (
        match user_id with
        | None -> Github_onboarding_pages.Login_required
        | Some _ ->
            let is_admin =
              session_field_opt request "is_admin" = Some "true"
            in
            if Project_onboarding.onboarding_available mode ~is_admin then
              Github_onboarding_pages.Ready
            else Github_onboarding_pages.Rollout_limited)
  in
  (* Chrome identity under the same validity rule as the access state: a
     session username without a valid positive user_id stays anonymous. *)
  let user = match user_id with None -> None | Some _ -> session_user request in
  (* no-store: the page reflects session identity, rollout mode, and
     one-time callback feedback; no-referrer keeps the feedback query out
     of outbound Referers. *)
  Dream.html
    ~headers:
      [ ("Cache-Control", "no-store"); ("Referrer-Policy", "no-referrer") ]
    (Github_onboarding_pages.bring_page ?user ~request ~access
       ~feedback:(bring_feedback_of_target (Dream.target request))
       ())

let make_oauth_callback_handler ~mode ~load_config ~load_credentials
    ~exchange_transport ~installations_transport request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no parsing, no configuration or credential read, no
         cookie access, no SQL, no GitHub. Still a clean redirect — the
         browser sits at a URL carrying a live code and state. *)
      Lwt.return (callback_failure ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      (* Sessionless like the setup return: this arrives on a cross-site
         top-level redirect from GitHub. Authorization is possession-based
         — the raw state plus the encrypted per-flow cookie — and the
         owning user comes back from the consumed state row, never from a
         Dream session. *)
      match parse_oauth_callback_target (Dream.target request) with
      | Error () -> Lwt.return (callback_failure ())
      | Ok (state, shape) -> (
          match load_config () with
          | Error _ ->
              (* Without configuration there is no cookie policy either,
                 so no deletion is attempted; credentials, SQL, and GitHub
                 stay untouched. *)
              Lwt.return (callback_failure ())
          | Ok config -> (
              match Github_onboarding_cookie.load config ~request ~state with
              | Error Github_onboarding_cookie.Missing ->
                  (* Another browser, or cleared data: nothing to delete,
                     and any other browser's cookie can still finish. No
                     credentials, SQL, or GitHub. *)
                  Lwt.return (callback_failure ())
              | Error Github_onboarding_cookie.Invalid ->
                  (* Undecryptable material is useless; delete it. Still no
                     credentials, SQL, or GitHub. *)
                  Lwt.return
                    (callback_failure_dropping config ~request ~state)
              | Ok data -> (
                  match shape with
                  | Authorization_rejected ->
                      (* The user said no at GitHub: end the flow without
                         credentials, SQL, or GitHub calls. The untouched
                         state row expires on its own. *)
                      Lwt.return
                        (callback_failure_dropping config ~request ~state)
                  | Authorization_granted code -> (
                      match load_credentials () with
                      | Error _ ->
                          (* Deployment problem with the state untouched:
                             keep the cookie so a manual refresh can retry
                             once the configuration is repaired. *)
                          Lwt.return (callback_failure ())
                      | Ok credentials ->
                          finish_authorization ~config ~credentials
                            ~exchange_transport ~installations_transport
                            ~request ~state ~data ~code)))))
