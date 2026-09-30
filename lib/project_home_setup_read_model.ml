(* Owner-authorized, read-only view of one permanent verified project by
   canonical slug. Authorization happens inside the SQL itself — slug,
   stewardship, and verified status together in one WHERE clause, never a
   load-then-check — and every unavailable state collapses to the same
   absent result, so slug probing cannot become an ownership or state
   oracle. The read is one statement, hence one PostgreSQL snapshot:
   project metadata and repositories can never disagree. Postgres is
   trusted as the persistence boundary, but the structural invariants the
   permanent page relies on (canonical slug and URLs, contiguous
   positions, primary rules, byte validity) are re-checked before any row
   is returned. See the .mli for the full contract. *)

open Lwt.Infix

type repository = {
  position : int;
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_primary : bool;
  is_archived : bool;
}

type project = {
  project_id : int64;
  name : string;
  slug : string;
  description : string option;
  website_url : string option;
  kind : Project_identity.kind;
  namespace_login : string;
  namespace_type : Github_user_installations.account_type;
  repositories : repository list;
}

let project_id (p : project) = p.project_id
let name (p : project) = p.name
let slug (p : project) = p.slug
let description (p : project) = p.description
let website_url (p : project) = p.website_url
let kind (p : project) = p.kind
let namespace_login (p : project) = p.namespace_login
let namespace_type (p : project) = p.namespace_type
let repositories (p : project) = p.repositories
let position (r : repository) = r.position
let full_name (r : repository) = r.full_name
let html_url (r : repository) = r.html_url
let repository_description (r : repository) = r.description
let default_branch (r : repository) = r.default_branch
let is_primary (r : repository) = r.is_primary
let is_archived (r : repository) = r.is_archived

type error =
  | Invalid_user_id
  | Invalid_slug
  | Inconsistent_data
  | Storage_error

(* The finalization store copies at most one complete draft snapshot, and
   the GitHub client caps a listing at twenty 100-entry pages. *)
let repository_limit = 2000

(* The permanent canonical slug shape — the same grammar
   Project_identity.create persists. Route values must already be
   canonical: nothing here lowercases, trims, or repairs, so an aliased
   spelling can never reach the database. *)
let canonical_slug value =
  let length = String.length value in
  let is_alnum c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  let rec check i =
    i >= length
    ||
    match value.[i] with
    | c when is_alnum c -> check (i + 1)
    | '-' -> i > 0 && is_alnum value.[i - 1] && check (i + 1)
    | _ -> false
  in
  length >= 1 && length <= 80 && is_alnum value.[length - 1] && check 0

(* Byte rules mirror the draft read model, whose validated snapshot the
   finalization store copied: owners and names are single URL path
   segments; branches are opaque but slash-separated names are legitimate;
   descriptions are free text barred only from NUL and other controls. *)
let valid_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let valid_branch value =
  String.length value > 0
  && String.for_all
       (fun byte -> Char.code byte > 0x20 && Char.code byte <> 0x7f)
       value

let valid_description value =
  String.for_all (fun byte -> Char.code byte >= 0x20) value

(* Structured reconstruction, exactly as the GitHub client built the value
   the finalization store copied — never string comparison against
   caller-influenced parts. *)
let canonical_html_url ~owner_login ~name =
  Uri.to_string
    (Uri.make ~scheme:"https" ~host:"github.com"
       ~path:("/" ^ owner_login ^ "/" ^ name)
       ())

(* The permanent table stores full_name only; both halves must be valid
   segments, which also guarantees exactly one '/'. *)
let split_full_name value =
  match String.index_opt value '/' with
  | None -> None
  | Some i ->
      let owner_login = String.sub value 0 i in
      let name = String.sub value (i + 1) (String.length value - i - 1) in
      if valid_segment owner_login && valid_segment name then
        Some (owner_login, name)
      else None

(* Going through Github_onboarding keeps the canonical database strings
   defined in exactly one place; its error message (which echoes the raw
   value) is deliberately dropped. *)
let account_type_of_db value =
  match Github_onboarding.account_type_of_string value with
  | Ok Github_onboarding.User -> Some Github_user_installations.User
  | Ok Github_onboarding.Organization ->
      Some Github_user_installations.Organization
  | Error _ -> None

(* Stewardship, slug, and verified status are authorized together; the
   steward primary key (project_id, user_id) makes the join row-unique.
   The repository join is LEFT on purpose: for a steward of an otherwise
   verified project the corrupted zero-repository state must be
   distinguishable (rows with all-NULL repository columns → option None)
   from plain absence (zero rows), while everyone else still gets zero
   rows. One statement means one consistent snapshot. *)
let repository_row =
  Caqti_type.(t2 (t4 int string string (option string)) (t3 string bool bool))

let load_for_steward_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int)
  ->* Caqti_type.(
        t2
          (t2
             (t4 int64 string string (option string))
             (t4 (option string) string string string))
          (option repository_row)))
    "SELECT p.id, p.name, p.slug, p.description, p.website_url, p.kind, \
     p.forge_namespace_login, p.forge_namespace_type, r.position, r.full_name, \
     r.html_url, r.description, r.default_branch, r.is_primary, r.is_archived \
     FROM open_source_projects p JOIN project_stewards s ON s.project_id = \
     p.id AND s.user_id = $2 LEFT JOIN project_repositories r ON r.project_id \
     = p.id WHERE p.slug = $1 AND p.verification_status = 'verified' ORDER BY \
     r.position"

(* Every structural rule one repository row must satisfy on its own.
   Errors are deliberately unit: which rule failed on which value must not
   travel. *)
let repository_of_row
    ( (position, full_name, html_url, description),
      (default_branch, is_primary, is_archived) ) =
  let valid =
    position > 0
    && (match split_full_name full_name with
      | None -> false
      | Some (owner_login, name) ->
          String.equal html_url (canonical_html_url ~owner_login ~name))
    && valid_branch default_branch
    &&
    match description with
    | None -> true
    | Some text -> valid_description text
  in
  if valid then
    Ok
      {
        position;
        full_name;
        html_url;
        description;
        default_branch;
        is_primary;
        is_archived;
      }
  else Error ()

let all_distinct values =
  List.length (List.sort_uniq compare values) = List.length values

(* Rows arrive ordered by position, so contiguity from 1 also rules out
   duplicate positions. *)
let contiguous_positions repositories =
  let rec check expected = function
    | [] -> true
    | (r : repository) :: rest ->
        r.position = expected && check (expected + 1) rest
  in
  check 1 repositories

(* Cross-row rules over one complete repository set; per-row rules have
   already passed. The primary rule depends on the project kind: a plain
   project has exactly one primary, every other kind zero or one. *)
let validate_repositories ~kind rows =
  let count = List.length rows in
  if count < 1 || count > repository_limit then Error ()
  else
    let rec convert acc = function
      | [] -> Ok (List.rev acc)
      | row :: rest -> (
          match repository_of_row row with
          | Error () -> Error ()
          | Ok r -> convert (r :: acc) rest)
    in
    match convert [] rows with
    | Error () -> Error ()
    | Ok repositories ->
        let primaries =
          List.length
            (List.filter (fun (r : repository) -> r.is_primary) repositories)
        in
        let primary_rule =
          match kind with
          | Project_identity.Project -> primaries = 1
          | _ -> primaries <= 1
        in
        if
          contiguous_positions repositories
          && all_distinct
               (List.map (fun (r : repository) -> r.full_name) repositories)
          && primary_rule
        then Ok repositories
        else Error ()

let project_of_rows
    ( (row_id, stored_name, stored_slug, stored_description),
      (stored_website, kind_string, login, type_string) ) raw_rows =
  match Project_identity.kind_of_string kind_string with
  | None -> Error ()
  | Some parsed_kind -> (
      match account_type_of_db type_string with
      | None -> Error ()
      | Some parsed_type -> (
          if
            Int64.compare row_id 0L <= 0
            || (not (canonical_slug stored_slug))
            || not (valid_segment login)
          then Error ()
          else
            match validate_repositories ~kind:parsed_kind raw_rows with
            | Error () -> Error ()
            | Ok validated ->
                Ok
                  {
                    project_id = row_id;
                    name = stored_name;
                    slug = stored_slug;
                    description = stored_description;
                    website_url = stored_website;
                    kind = parsed_kind;
                    namespace_login = login;
                    namespace_type = parsed_type;
                    repositories = validated;
                  }))

let load_for_steward (module C : Caqti_lwt.CONNECTION) ~user_id ~slug =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_slug slug) then Lwt.return (Error Invalid_slug)
  else
    C.collect_list load_for_steward_query (slug, user_id) >|= function
    | Error _ -> Error Storage_error
    | Ok [] -> Ok None
    | Ok ((identity, _) :: _ as rows) -> (
        (* An all-NULL repository half means the LEFT JOIN found no
           permanent repository rows: the caller stewards an otherwise
           verified project that is corrupt, which must be an error, never
           an empty view. Anyone else already got Ok None above. *)
        let repository_rows =
          List.fold_right
            (fun (_, repo) acc ->
              match (acc, repo) with
              | None, _ | _, None -> None
              | Some rest, Some row -> Some (row :: rest))
            rows (Some [])
        in
        match repository_rows with
        | None -> Error Inconsistent_data
        | Some raw_rows -> (
            match project_of_rows identity raw_rows with
            | Error () -> Error Inconsistent_data
            | Ok view -> Ok (Some view)))
