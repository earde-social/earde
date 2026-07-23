(* Public-repository listing for a verified GitHub App installation. Pages
   through GET /user/installations/<id>/repositories with the OAuth user
   access token, fully validates every page, keeps only repositories that
   are simultaneously non-private and "public", and requires each entry to
   be owned by the verified installation's account ID. The transport is the
   one already used by Github_user_installations, so callers can neither
   steer the credential-bearing request nor observe more than the closed
   error type. Every error constructor is payload-free except the HTTP
   status integer, and nothing in this module logs — see the .mli for the
   privacy contract. *)

type repository = {
  repository_id : int64;
  owner_id : int64;
  owner_login : string;
  name : string;
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_archived : bool;
}

(* Invariant: non-empty. list_public answers No_public_repositories rather
   than ever building an empty set. *)
type repository_set = repository list

let repositories set = set
let repository_id repository = repository.repository_id
let owner_id repository = repository.owner_id
let owner_login repository = repository.owner_login
let name repository = repository.name
let full_name repository = repository.full_name
let html_url repository = repository.html_url
let description repository = repository.description
let default_branch repository = repository.default_branch
let is_archived repository = repository.is_archived

module type TRANSPORT = Github_user_installations.TRANSPORT

(* The installations client's production transport, re-exported rather than
   duplicated: one GET, no redirects, ten-second budget, bounded body,
   payload-free failure, propagated Lwt.Canceled. *)
module Cohttp_transport = Github_user_installations.Cohttp_transport

type error =
  | Transport_error
  | Unexpected_http_status of int
  | Invalid_response
  | No_public_repositories
  | Pagination_limit

let per_page = 100

(* Twenty pages of one hundred: a 2,000-repository ceiling per onboarding
   flow, and a hard stop so a hostile total_count cannot drive unbounded
   requests. *)
let max_pages = 20

(* Fixed scheme and host: deriving the API endpoint from configuration or
   input would let a misconfiguration or an attacker aim the user token at
   an arbitrary host. The path ID comes only from the abstract verified
   installation, so it is already a proven positive int64. *)
let page_uri ~installation_id page =
  Uri.make ~scheme:"https" ~host:"api.github.com"
    ~path:
      (Printf.sprintf "/user/installations/%Ld/repositories" installation_id)
    ~query:
      [ ("per_page", [ string_of_int per_page ]); ("page", [ string_of_int page ]) ]
    ()

(* The token rides only in the Authorization header, never in the URI. *)
let request_headers token_set =
  [
    ("accept", "application/vnd.github+json");
    ( "authorization",
      "Bearer " ^ Github_oauth_token_exchange.access_token token_set );
    ("x-github-api-version", "2026-03-10");
    ("user-agent", "Earde-GitHub-Onboarding");
  ]

let recognized_page_keys = [ "total_count"; "repositories" ]

let recognized_repository_keys =
  [
    "id"; "name"; "full_name"; "owner"; "private"; "visibility";
    "description"; "default_branch"; "archived";
  ]

let occurrences fields key =
  List.length (List.filter (fun (k, _) -> String.equal k key) fields)

(* Exact-value int64 only: floats, numeric strings, null, booleans, and
   literals outside the int64 range are all rejected. `Intlit` carries
   integers Yojson could not fit in an OCaml int, whose exactness
   Int64.of_string_opt then decides. *)
let int64_of_json = function
  | `Int n -> Some (Int64.of_int n)
  | `Intlit literal -> Int64.of_string_opt literal
  | _ -> None

(* GitHub issues repository and account IDs as positive integers; zero or
   negative values only arise from a malformed or hostile page. *)
let positive_id_of_json json =
  match int64_of_json json with
  | Some id when Int64.compare id 0L > 0 -> Some id
  | _ -> None

(* Exactly-once lookup: a missing recognized field and a duplicated one
   both make the object ambiguous, so both collapse to None. *)
let unique_field fields key =
  match List.filter (fun (k, _) -> String.equal k key) fields with
  | [ (_, json) ] -> Some json
  | _ -> None

(* Owner logins and repository names become single URL path segments, so
   ASCII whitespace, NUL, other control bytes, DEL, and '/' are rejected
   outright rather than trimmed or escaped; every other byte passes through
   untouched. GitHub, not this module, owns the grammar beyond that. *)
let valid_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Branch names are opaque snapshots, never turned into a URL or path by
   this module, and slash-separated names like release/v1 are legitimate —
   so '/' is allowed and only ASCII whitespace, NUL, other control bytes,
   and DEL are rejected. No trimming, normalization, or branch-name grammar
   beyond that; any later consumer that embeds the value in a URL or
   command must encode or validate it at that boundary. *)
let valid_branch value =
  String.length value > 0
  && String.for_all
       (fun byte -> Char.code byte > 0x20 && Char.code byte <> 0x7f)
       value

(* Descriptions are free text: only NUL and the other ASCII control bytes
   are rejected; accepted bytes — UTF-8 and punctuation included — are
   preserved exactly, untrimmed. *)
let valid_description value =
  String.for_all (fun byte -> Char.code byte >= 0x20) value

(* The browser URL is never taken from the response: it is rebuilt from the
   already-validated owner login and repository name, so it can only ever
   point into github.com, with no query, fragment, or userinfo. *)
let canonical_html_url ~owner_login ~name =
  Uri.to_string
    (Uri.make ~scheme:"https" ~host:"github.com"
       ~path:("/" ^ owner_login ^ "/" ^ name)
       ())

let ( let* ) = Option.bind

(* Exactly one recognized "id" and "login"; other owner fields are ignored
   and dropped. The owner ID must equal the verified installation's account
   ID — the stable ownership check. The login is deliberately not compared
   against the installation's stored login, because GitHub logins can be
   renamed independently of the immutable account ID. *)
let parse_owner ~account_id = function
  | `Assoc fields ->
      let* id_json = unique_field fields "id" in
      let* owner_id = positive_id_of_json id_json in
      let* login =
        match unique_field fields "login" with
        | Some (`String login) when valid_segment login -> Some login
        | _ -> None
      in
      if Int64.equal owner_id account_id then Some (owner_id, login)
      else None
  | _ -> None

(* A structurally valid entry plus whether it may be returned. Non-public
   entries are validated exactly as strictly, then die in [absorb]. *)
type validated_entry = { repo : repository; is_public : bool }

let parse_repository ~account_id = function
  | `Assoc fields ->
      (* A duplicated recognized field makes the object ambiguous; unknown
         fields — including any response-supplied html_url — are ignored
         and dropped, never retained. *)
      if
        List.exists
          (fun key -> occurrences fields key > 1)
          recognized_repository_keys
      then None
      else
        let* repository_id =
          let* json = unique_field fields "id" in
          positive_id_of_json json
        in
        let* name =
          match unique_field fields "name" with
          | Some (`String value) when valid_segment value -> Some value
          | _ -> None
        in
        let* owner_id, owner_login =
          let* json = unique_field fields "owner" in
          parse_owner ~account_id json
        in
        let* full_name =
          match unique_field fields "full_name" with
          | Some (`String value)
            when String.equal value (owner_login ^ "/" ^ name) ->
              Some value
          | _ -> None
        in
        let* is_private =
          match unique_field fields "private" with
          | Some (`Bool value) -> Some value
          | _ -> None
        in
        let* visibility =
          match unique_field fields "visibility" with
          | Some (`String value) -> Some value
          | _ -> None
        in
        let* description =
          match unique_field fields "description" with
          | Some `Null -> Some None
          | Some (`String value) when valid_description value ->
              Some (Some value)
          | _ -> None
        in
        let* default_branch =
          match unique_field fields "default_branch" with
          | Some (`String value) when valid_branch value -> Some value
          | _ -> None
        in
        let* is_archived =
          match unique_field fields "archived" with
          | Some (`Bool value) -> Some value
          | _ -> None
        in
        Some
          {
            repo =
              {
                repository_id;
                owner_id;
                owner_login;
                name;
                full_name;
                html_url = canonical_html_url ~owner_login ~name;
                description;
                default_branch;
                is_archived;
              };
            (* Both signals must agree: internal repositories and any
               private/visibility disagreement are all non-public. *)
            is_public = (not is_private) && String.equal visibility "public";
          }
  | _ -> None

(* Every entry — public or not — must validate, or the page is rejected
   whole; a page that was not fully understood can never contribute
   repositories. *)
let rec validated_entries ~account_id validated = function
  | [] -> Some (List.rev validated)
  | entry :: rest -> (
      match parse_repository ~account_id entry with
      | Some parsed ->
          validated_entries ~account_id (parsed :: validated) rest
      | None -> None)

let parse_page ~account_id body =
  match Yojson.Safe.from_string body with
  | exception _ -> Error Invalid_response
  | `Assoc fields -> (
      (* Yojson preserves duplicate keys in `Assoc`; a duplicated recognized
         key makes the response ambiguous, so it is rejected before any
         field is interpreted. Unknown top-level keys stay ignored. *)
      if
        List.exists
          (fun key -> occurrences fields key > 1)
          recognized_page_keys
      then Error Invalid_response
      else
        match
          ( List.assoc_opt "total_count" fields,
            List.assoc_opt "repositories" fields )
        with
        | Some total_json, Some (`List entries)
          when List.length entries <= per_page -> (
            match
              ( int64_of_json total_json,
                validated_entries ~account_id [] entries )
            with
            | Some total_count, Some validated
              when Int64.compare total_count 0L >= 0 ->
                Ok (total_count, validated)
            | _ -> Error Invalid_response)
        | _ -> Error Invalid_response)
  | _ -> Error Invalid_response

(* GitHub must not repeat a repository: a duplicate ID or duplicate exact
   full name — anywhere across the whole listing, non-public entries
   included — is treated as an invalid response rather than silently
   deduplicated. Only public entries are kept; everything about a
   non-public entry ends here. *)
let rec absorb seen publics = function
  | [] -> Some (seen, publics)
  | { repo; is_public } :: rest ->
      if
        List.exists
          (fun previous ->
            Int64.equal previous.repository_id repo.repository_id
            || String.equal previous.full_name repo.full_name)
          seen
      then None
      else
        absorb (repo :: seen)
          (if is_public then repo :: publics else publics)
          rest

let list_public ~transport:(module Transport : TRANSPORT) ~token_set
    ~installation =
  let installation_id =
    Github_user_installations.installation_id installation
  in
  let account_id = Github_user_installations.account_id installation in
  let headers = request_headers token_set in
  let rec fetch page seen publics =
    let%lwt result =
      Transport.get ~uri:(page_uri ~installation_id page) ~headers
    in
    match result with
    | Error () -> Lwt.return (Error Transport_error)
    | Ok (200, body) -> (
        match parse_page ~account_id body with
        | Error error -> Lwt.return (Error error)
        | Ok (total_count, validated) -> (
            match absorb seen publics validated with
            | None -> Lwt.return (Error Invalid_response)
            | Some (seen, publics) ->
                let complete =
                  (* A short page is the definitive end of the listing, and
                     so is having covered total_count with full pages. *)
                  List.length validated < per_page
                  || Int64.compare
                       (Int64.mul (Int64.of_int page) (Int64.of_int per_page))
                       total_count
                     >= 0
                in
                if complete then (
                  match List.rev publics with
                  | [] -> Lwt.return (Error No_public_repositories)
                  | set -> Lwt.return (Ok set))
                else if page < max_pages then fetch (page + 1) seen publics
                else
                  (* The scan stopped because of our own page cap, not
                     because the listing ended — returning a set here would
                     misreport an incomplete scan as complete. *)
                  Lwt.return (Error Pagination_limit)))
    | Ok (status, _) ->
        (* The body of a non-200 answer is never parsed or preserved; the
           status integer is the only remote datum an error may carry. *)
        Lwt.return (Error (Unexpected_http_status status))
  in
  fetch 1 [] []
