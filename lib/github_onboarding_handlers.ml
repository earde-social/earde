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
