(* Same-origin browser check shared by the state-changing POST endpoints of
   the GitHub onboarding and project-setup flows. Moved verbatim from
   Github_onboarding_handlers so the two never drift; the start handler's
   behavior is pinned by its existing origin-gate regression tests. *)

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

(* Same-origin browser check for a state-changing POST. A present Origin
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
