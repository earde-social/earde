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

(* Strict single-occurrence extraction of one raw query value from the
   original request target. Dream.query silently tolerates duplicate keys,
   so the target is split by hand: only the substring after the first '?',
   stopped at a raw '#', components on raw '&', each component at its first
   '='. Keys are case-sensitive; unrelated parameters are ignored (GitHub
   may append optional setup metadata). The value is used byte-for-byte —
   the canonical state alphabet needs no percent-decoding, and decoding
   would create aliases of one stored state. *)
let raw_query_value ~key target =
  match String.index_opt target '?' with
  | None -> Error ()
  | Some q -> (
      let query = String.sub target (q + 1) (String.length target - q - 1) in
      let query =
        match String.index_opt query '#' with
        | None -> query
        | Some h -> String.sub query 0 h
      in
      let values =
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
      in
      match values with
      | [ Some value ] when not (String.equal value "") -> Ok value
      | _ -> Error ())

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
