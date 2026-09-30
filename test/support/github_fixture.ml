(* GitHub client fixtures: canonical tokens, PKCE material, App
   configuration, the onboarding cookie secret, and fake transports for
   the token exchange, installation and repository listings. No network. *)

module GOC = Earde.Github_onboarding_crypto
module GPK = Earde.Github_onboarding_pkce
module GAC = Earde.Github_app_config
module GCS = Earde.Github_oauth_credentials
module GSD = Earde.Github_onboarding_session_data
module GCK = Earde.Github_onboarding_cookie
module GTE = Earde.Github_oauth_token_exchange
module GUI = Earde.Github_user_installations
module GUR = Earde.Github_user_installation_repositories

(* Canonical means: decodes, is exactly 32 bytes, and re-encodes to the
   same string (so unpadded, no whitespace, zero trailing bits). *)
let goc_canonical_32 encoded =
  match Dream.from_base64url encoded with
  | None -> false
  | Some raw ->
      String.length raw = 32 && String.equal (Dream.to_base64url raw) encoded

(* Deterministic 32-byte fixture token; safe to print, unlike generated
   material. *)
let goc_fixture byte = Dream.to_base64url (String.make 32 byte)

let goc_state_exn input =
  match GOC.state_of_callback input with
  | Ok state -> state
  | Error GOC.Invalid_format -> Alcotest.fail "fixture did not parse as state"

let goc_state_hash input =
  GOC.state_hash_to_string (GOC.hash_state (goc_state_exn input))

let goc_is_hex64 s =
  String.length s = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let gpk_verifier_exn input =
  match GPK.verifier_of_string input with
  | Ok verifier -> verifier
  | Error GPK.Invalid_format ->
      Alcotest.fail "fixture did not parse as verifier"

(* Baseline valid production configuration; each case overrides one field. *)
let gac_of_values ?(origin = Some "https://earde.com")
    ?(slug = Some "earde-connect") ?(client = Some "Iv1.8a61f9b3a7aba766")
    ?(setup = Some "https://earde.com/integrations/github/install/return")
    ?(callback =
      Some "https://earde.com/integrations/github/authorize/callback") () =
  GAC.of_values ~public_origin:origin ~app_slug:slug ~client_id:client
    ~setup_url:setup ~callback_url:callback

let gac_ok_exn label result =
  match result with
  | Ok config -> config
  | Error e ->
      Alcotest.failf "%s: rejected with %s" label (GAC.string_of_error e)

let gou_keys uri = List.map fst (Uri.query uri)

let gou_entries key uri =
  List.filter (fun (k, _) -> String.equal k key) (Uri.query uri)

(* The decoded value of [key], requiring the key to appear exactly once
   with exactly one value. *)
let gou_single label key uri =
  match gou_entries key uri with
  | [ (_, [ v ]) ] -> v
  | _ -> Alcotest.failf "%s: expected exactly one %s value" label key

let gou_authorization_keys =
  [ "client_id"; "redirect_uri"; "state"; "code_challenge";
    "code_challenge_method" ]

let gsd_binding_hash data =
  GOC.session_binding_hash_to_string (GSD.session_binding_hash data)

let gsd_verifier data = GPK.verifier_to_string (GSD.verifier data)

let gsd_challenge data = GPK.challenge_to_string (GSD.code_challenge data)

let gsd_cookie_prefix = "earde.github_onboarding.v1."

let cookie_secret = "earde-test-secret-github-onboarding-cookie"

let gck_https_config () = gac_ok_exn "https cookie config" (gac_of_values ())

(* Test-only browser simulation: the Cookie header a browser would send
   back after receiving the given Set-Cookie name/value pairs. *)
let gck_cookie_header pairs =
  String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) pairs)

(* Runs [f] on a fresh mock request carrying the fixed test secret, so a
   value encrypted in one simulated request decrypts in another. *)
let gck_with_request ?cookies f =
  let headers =
    match cookies with
    | None | Some [] -> []
    | Some pairs -> [ ("Cookie", gck_cookie_header pairs) ]
  in
  let result = ref None in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.set_secret cookie_secret
         (fun request ->
           result := Some (f request);
           Dream.respond "")
         (Dream.request ~headers ""))
  in
  match !result with
  | Some value -> value
  | None -> Alcotest.fail "secret middleware did not run the handler"

let gck_store_headers config state data =
  gck_with_request (fun request ->
      let response = Dream.response "" in
      GCK.store config ~request ~response ~state data;
      Dream.headers response "Set-Cookie")

(* Splits one Set-Cookie header into the cookie pair plus its attributes;
   attribute names and values are lowercased for case-insensitive matching
   (attribute values here are policy keywords, never material). *)
let gck_parse_set_cookie header =
  match String.split_on_char ';' header with
  | [] -> Alcotest.fail "empty Set-Cookie header"
  | pair :: attributes ->
      let name, value =
        match String.index_opt pair '=' with
        | None -> Alcotest.fail "Set-Cookie without a name=value pair"
        | Some i ->
            ( String.sub pair 0 i,
              String.sub pair (i + 1) (String.length pair - i - 1) )
      in
      let attributes =
        List.map
          (fun attribute ->
            let attribute =
              String.lowercase_ascii (String.trim attribute)
            in
            match String.index_opt attribute '=' with
            | None -> (attribute, "")
            | Some i ->
                ( String.sub attribute 0 i,
                  String.sub attribute (i + 1)
                    (String.length attribute - i - 1) ))
          attributes
      in
      (name, value, attributes)

let gck_single_set_cookie label headers =
  match headers with
  | [ header ] -> gck_parse_set_cookie header
  | headers ->
      Alcotest.failf "%s: expected exactly one Set-Cookie, got %d" label
        (List.length headers)

let gck_stored label config state data =
  gck_single_set_cookie label (gck_store_headers config state data)

let gte_config () = gac_ok_exn "token exchange config" (gac_of_values ())

let gte_client_secret = "gte+SECRET/fixture=42"

let gte_credentials () =
  match GCS.of_values ~client_secret:(Some gte_client_secret) with
  | Ok credentials -> credentials
  | Error _ -> Alcotest.fail "client secret fixture rejected"

let gte_verifier_string = goc_fixture 'W'

let gte_verifier () = gpk_verifier_exn gte_verifier_string

(* Reserved punctuation on purpose: byte preservation must survive form
   encoding, and no fixed alphabet may be imposed on the code. *)
let gte_code_string = "c0de&=?#%+/~._-!$'()*,;:@[]"

let gte_code_exn raw =
  match GTE.authorization_code_of_callback raw with
  | Ok code -> code
  | Error GTE.Invalid_code -> Alcotest.fail "code fixture rejected"

let gte_show_error = function
  | GTE.Transport_error -> "Transport_error"
  | GTE.Unexpected_http_status status ->
      "Unexpected_http_status " ^ string_of_int status
  | GTE.OAuth_rejected -> "OAuth_rejected"
  | GTE.Invalid_response -> "Invalid_response"

(* Fake transport answering [result]; the request is captured for
   structural assertions. *)
let gte_transport result captured =
  (module struct
    let post ~uri ~headers ~body =
      captured := Some (uri, headers, body);
      Lwt.return result
  end : GTE.TRANSPORT)

let gte_exchange transport =
  Lwt_main.run
    (GTE.exchange ~transport ~config:(gte_config ())
       ~credentials:(gte_credentials ())
       ~code:(gte_code_exn gte_code_string)
       ~verifier:(gte_verifier ()))

let gte_tokens_exn label outcome =
  match outcome with
  | Ok tokens -> tokens
  | Error e ->
      Alcotest.failf "%s: exchange failed with %s" label (gte_show_error e)

(* Fake transport answering scripted responses in order; every request's
   (uri, headers) is captured for structural assertions. Being called more
   times than scripted is itself a failure. *)
let gui_transport responses captured =
  let remaining = ref responses in
  (module struct
    let get ~uri ~headers =
      captured := !captured @ [ (uri, headers) ];
      match !remaining with
      | [] -> Alcotest.fail "transport called more times than scripted"
      | response :: rest ->
          remaining := rest;
          Lwt.return response
  end : GUI.TRANSPORT)

let gui_show_error = function
  | GUI.Invalid_installation_id -> "Invalid_installation_id"
  | GUI.Transport_error -> "Transport_error"
  | GUI.Unexpected_http_status status ->
      "Unexpected_http_status " ^ string_of_int status
  | GUI.Invalid_response -> "Invalid_response"
  | GUI.Installation_not_accessible -> "Installation_not_accessible"
  | GUI.Pagination_limit -> "Pagination_limit"

let gui_verified_exn label outcome =
  match outcome with
  | Ok verified -> verified
  | Error e ->
      Alcotest.failf "%s: verification failed with %s" label
        (gui_show_error e)

(* [count] sequential filler installation IDs starting at [from]. *)
let gui_ids ~from count =
  List.init count (fun i -> Int64.add from (Int64.of_int i))

(* A fully valid installation entry: account ID and login are derived from
   the installation ID so filler entries stay distinct. *)
let gui_entry id =
  Printf.sprintf
    {|{"id":%Ld,"account":{"id":%Ld,"login":"owner-%Ld"},"target_type":"Organization"}|}
    id (Int64.add id 1L) id

let gui_entries ids = String.concat "," (List.map gui_entry ids)

let gui_body ~total ids =
  Printf.sprintf {|{"total_count":%d,"installations":[%s]}|} total
    (gui_entries ids)

(* A single-entry page whose entry JSON is supplied raw, so each rejection
   case varies exactly the malformed part. *)
let gui_entry_page entry_json =
  Printf.sprintf {|{"total_count":1,"installations":[%s]}|} entry_json

(* Entry and account bodies with substitutable raw JSON per field, so a
   rejection case changes one field and inherits valid defaults for the
   rest. *)
let gui_raw_entry ?(id = "424242")
    ?(account = {|{"id":424243,"login":"owner-424242"}|})
    ?(target = {|"Organization"|}) () =
  Printf.sprintf {|{"id":%s,"account":%s,"target_type":%s}|} id account
    target

let gui_page_of ~total ids = Ok (200, gui_body ~total ids)

(* Page number a captured request asked for, decoded from its query. *)
let gui_requested_page label (uri, _) =
  match Uri.get_query_param uri "page" with
  | Some page -> page
  | None -> Alcotest.failf "%s: request has no page parameter" label

let gur_account_id = 9099L

let gur_login = "earde-owner"

(* Fake transport answering scripted responses in order; every request's
   (uri, headers) is captured for structural assertions. Being called more
   times than scripted is itself a failure. *)
let gur_transport responses captured =
  let remaining = ref responses in
  (module struct
    let get ~uri ~headers =
      captured := !captured @ [ (uri, headers) ];
      match !remaining with
      | [] -> Alcotest.fail "transport called more times than scripted"
      | response :: rest ->
          remaining := rest;
          Lwt.return response
  end : GUR.TRANSPORT)

let gur_show_error = function
  | GUR.Transport_error -> "Transport_error"
  | GUR.Unexpected_http_status status ->
      "Unexpected_http_status " ^ string_of_int status
  | GUR.Invalid_response -> "Invalid_response"
  | GUR.No_public_repositories -> "No_public_repositories"
  | GUR.Pagination_limit -> "Pagination_limit"

(* A fully valid repository entry owned by the fixture account.
   [description] is raw JSON so null and UTF-8 strings both fit; full_name
   is derived from the owner login and name exactly as GitHub would. *)
let gur_repo ?(owner_id = gur_account_id) ?(owner_login = gur_login)
    ?(private_flag = false) ?(visibility = "public") ?(description = "null")
    ?(default_branch = "main") ?(archived = false) ~id ~name () =
  Printf.sprintf
    {|{"id":%Ld,"name":"%s","full_name":"%s/%s","owner":{"id":%Ld,"login":"%s"},"private":%b,"visibility":"%s","description":%s,"default_branch":"%s","archived":%b}|}
    id name owner_login name owner_id owner_login private_flag visibility
    description default_branch archived

(* Deliberately distinctive non-public fixture strings, asserted only
   through boolean containment so they can never reach test output. *)
let gur_private_name = "secret-repo-fixture"

let gur_private_description = "SECRET-PRIVATE-DESCRIPTION-FIXTURE"

let gur_body ~total entries =
  Printf.sprintf {|{"total_count":%d,"repositories":[%s]}|} total
    (String.concat "," entries)
