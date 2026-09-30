(* Owner-authorized, read-only view of project-onboarding drafts. Both
   entry points authorize inside the SQL itself — the draft id is never
   fetched first and checked in OCaml — and every unavailable state
   collapses to the same absent result, so draft-id probing cannot become
   an ownership or state oracle. Each read is one statement, hence one
   PostgreSQL snapshot: summary and repositories can never disagree with
   each other, and a concurrent verified refresh can never interleave.
   Postgres is trusted as the persistence boundary, but the structural
   invariants handlers will rely on (canonical names and URLs, byte
   validity, contiguous positions, selection rules) are re-checked before
   any row is returned. See the .mli for the full contract. *)

open Lwt.Infix

type repository = {
  snapshot_id : int64;
  position : int;
  github_repository_id : int64;
  github_owner_id : int64;
  owner_login : string;
  name : string;
  full_name : string;
  html_url : string;
  description : string option;
  default_branch : string;
  is_archived : bool;
  is_selected : bool;
  is_primary : bool;
}

type draft_summary = {
  draft_id : int64;
  account_login : string;
  account_type : Github_user_installations.account_type;
  repository_count : int;
  selected_repository_count : int;
  has_primary_repository : bool;
}

type draft_view = { summary : draft_summary; repositories : repository list }

let draft_id (s : draft_summary) = s.draft_id
let account_login (s : draft_summary) = s.account_login
let account_type (s : draft_summary) = s.account_type
let repository_count (s : draft_summary) = s.repository_count
let selected_repository_count (s : draft_summary) = s.selected_repository_count
let has_primary_repository (s : draft_summary) = s.has_primary_repository
let summary (v : draft_view) = v.summary
let repositories (v : draft_view) = v.repositories
let snapshot_id (r : repository) = r.snapshot_id
let position (r : repository) = r.position
let github_repository_id (r : repository) = r.github_repository_id
let github_owner_id (r : repository) = r.github_owner_id
let owner_login (r : repository) = r.owner_login
let name (r : repository) = r.name
let full_name (r : repository) = r.full_name
let html_url (r : repository) = r.html_url
let description (r : repository) = r.description
let default_branch (r : repository) = r.default_branch
let is_archived (r : repository) = r.is_archived
let is_selected (r : repository) = r.is_selected
let is_primary (r : repository) = r.is_primary

type error =
  | Invalid_user_id
  | Invalid_draft_id
  | Inconsistent_data
  | Storage_error

(* The GitHub client caps a complete listing at twenty 100-entry pages, so
   no honest snapshot can exceed this. *)
let snapshot_limit = 2000

(* Byte rules mirror the GitHub client, the write-side gatekeeper: logins
   and names are single URL path segments (no whitespace, controls, DEL,
   or '/'); branches are opaque but slash-separated names are legitimate;
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

(* Structured reconstruction, exactly as the client built the stored
   value — never string comparison against caller-influenced parts. *)
let canonical_html_url ~owner_login ~name =
  Uri.to_string
    (Uri.make ~scheme:"https" ~host:"github.com"
       ~path:("/" ^ owner_login ^ "/" ^ name)
       ())

(* Going through Github_onboarding keeps the canonical database strings
   defined in exactly one place; its error message (which echoes the raw
   value) is deliberately dropped. *)
let account_type_of_db value =
  match Github_onboarding.account_type_of_string value with
  | Ok Github_onboarding.User -> Some Github_user_installations.User
  | Ok Github_onboarding.Organization ->
      Some Github_user_installations.Organization
  | Error _ -> None

(* Availability, shared by both queries: an unexpired active draft backed
   by a live installation. connected_by_user_id is provenance and never
   appears here; d.user_id is the only owner. *)
let availability_sql =
  "d.status = 'active' AND d.expires_at > NOW() AND i.status = 'active' AND \
   i.revoked_at IS NULL"

(* The INNER JOIN on snapshot rows silently drops a corrupted zero-repo
   draft from the list — it is unusable, and listing surfaces no
   corruption signal. Counts are aggregated in the same statement as the
   filter, so they can never describe a different snapshot than the one
   that qualified the draft. *)
let list_available_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->* Caqti_type.(t2 (t3 int64 string string) (t3 int int int)))
    ("SELECT d.id, i.github_account_login, i.github_account_type, \
      COUNT(*)::int, COUNT(*) FILTER (WHERE r.is_selected)::int, COUNT(*) \
      FILTER (WHERE r.is_primary)::int FROM project_onboarding_drafts d JOIN \
      github_installations i ON i.id = d.github_installation_record_id JOIN \
      project_onboarding_draft_repositories r ON r.draft_id = d.id WHERE \
      d.user_id = $1 AND " ^ availability_sql
   ^ " GROUP BY d.id, i.id ORDER BY d.updated_at DESC, d.id DESC")

(* Caqti row type for one snapshot row, nested per the house convention. *)
let repository_row =
  Caqti_type.(
    t2
      (t2 (t2 int64 int) (t2 int64 int64))
      (t2
         (t4 string string string string)
         (t2 (t2 (option string) string) (t3 bool bool bool))))

(* Draft id and user id are authorized together in the WHERE clause. The
   snapshot join is LEFT on purpose: for the owner of an otherwise
   available draft the corrupted zero-repository state must be
   distinguishable (one row, all-NULL repository columns → option None)
   from plain absence (zero rows), while everyone else still gets zero
   rows. One statement means one consistent snapshot of draft, account,
   and repositories. *)
let load_available_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int64 int)
  ->* Caqti_type.(t2 (t3 int64 string string) (option repository_row)))
    ("SELECT d.id, i.github_account_login, i.github_account_type, r.id, \
      r.position, r.github_repository_id, r.github_owner_id, r.owner_login, \
      r.name, r.full_name, r.html_url, r.description, r.default_branch, \
      r.is_archived, r.is_selected, r.is_primary FROM \
      project_onboarding_drafts d JOIN github_installations i ON i.id = \
      d.github_installation_record_id LEFT JOIN \
      project_onboarding_draft_repositories r ON r.draft_id = d.id WHERE d.id \
      = $1 AND d.user_id = $2 AND " ^ availability_sql ^ " ORDER BY r.position"
    )

(* Every structural rule one row must satisfy on its own. Errors are
   deliberately unit: which rule failed on which value must not travel. *)
let repository_of_row
    ( ((snapshot_id, position), (github_repository_id, github_owner_id)),
      ( (owner_login, name, full_name, html_url),
        ((description, default_branch), (is_archived, is_selected, is_primary))
      ) ) =
  let valid =
    Int64.compare snapshot_id 0L > 0
    && position > 0
    && Int64.compare github_repository_id 0L > 0
    && Int64.compare github_owner_id 0L > 0
    && valid_segment owner_login && valid_segment name
    && String.equal full_name (owner_login ^ "/" ^ name)
    && String.equal html_url (canonical_html_url ~owner_login ~name)
    && valid_branch default_branch
    && (match description with
      | None -> true
      | Some text -> valid_description text)
    && ((not is_primary) || is_selected)
  in
  if valid then
    Ok
      {
        snapshot_id;
        position;
        github_repository_id;
        github_owner_id;
        owner_login;
        name;
        full_name;
        html_url;
        description;
        default_branch;
        is_archived;
        is_selected;
        is_primary;
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

(* Cross-row rules over one complete snapshot; per-row rules have already
   passed. Position is never an identity — snapshot_id, the GitHub id, and
   the full name each identify a row and must each be unique. *)
let validate_snapshot rows =
  let count = List.length rows in
  if count < 1 || count > snapshot_limit then Error ()
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
        if
          contiguous_positions repositories
          && all_distinct
               (List.map (fun (r : repository) -> r.snapshot_id) repositories)
          && all_distinct
               (List.map
                  (fun (r : repository) -> r.github_repository_id)
                  repositories)
          && all_distinct
               (List.map (fun (r : repository) -> r.full_name) repositories)
          && List.length
               (List.filter (fun (r : repository) -> r.is_primary) repositories)
             <= 1
        then Ok repositories
        else Error ()

(* Summary invariants for the aggregate path. The per-row
   primary-implies-selected CHECK makes "a primary implies a selection"
   hold, but the counts arrived through SQL aggregation, so it is
   re-checked here rather than assumed. *)
let summary_of_counts (draft_row_id, login, type_string)
    (repo_count, selected_count, primary_count) =
  if
    repo_count < 1
    || repo_count > snapshot_limit
    || selected_count < 0
    || selected_count > repo_count
    || primary_count < 0 || primary_count > 1
    || (primary_count = 1 && selected_count < 1)
  then Error ()
  else
    match account_type_of_db type_string with
    | None -> Error ()
    | Some parsed_type ->
        Ok
          {
            draft_id = draft_row_id;
            account_login = login;
            account_type = parsed_type;
            repository_count = repo_count;
            selected_repository_count = selected_count;
            has_primary_repository = primary_count = 1;
          }

let list_available (module C : Caqti_lwt.CONNECTION) ~user_id =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else
    C.collect_list list_available_query user_id >|= function
    | Error _ -> Error Storage_error
    | Ok rows ->
        (* One bad row poisons the whole list — a partial list would
           silently hide a draft the user owns. *)
        let rec build acc = function
          | [] -> Ok (List.rev acc)
          | (identity, counts) :: rest -> (
              match summary_of_counts identity counts with
              | Error () -> Error Inconsistent_data
              | Ok s -> build (s :: acc) rest)
        in
        build [] rows

let load_available (module C : Caqti_lwt.CONNECTION) ~user_id ~draft_id =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if Int64.compare draft_id 0L <= 0 then
    Lwt.return (Error Invalid_draft_id)
  else
    C.collect_list load_available_query (draft_id, user_id) >|= function
    | Error _ -> Error Storage_error
    | Ok [] -> Ok None
    | Ok ((identity, _) :: _ as rows) -> (
        (* An all-NULL repository half means the LEFT JOIN found no
           snapshot rows: the caller owns an otherwise available draft
           that is corrupt, which must be an error, never an empty view.
           Anyone else already got Ok None above. *)
        let snapshot_rows =
          List.fold_right
            (fun (_, repo) acc ->
              match (acc, repo) with
              | None, _ | _, None -> None
              | Some rest, Some row -> Some (row :: rest))
            rows (Some [])
        in
        match snapshot_rows with
        | None -> Error Inconsistent_data
        | Some raw_rows -> (
            match validate_snapshot raw_rows with
            | Error () -> Error Inconsistent_data
            | Ok validated -> (
                let draft_row_id, login, type_string = identity in
                let selected =
                  List.length
                    (List.filter
                       (fun (r : repository) -> r.is_selected)
                       validated)
                in
                let counts =
                  ( List.length validated,
                    selected,
                    List.length
                      (List.filter
                         (fun (r : repository) -> r.is_primary)
                         validated) )
                in
                match
                  summary_of_counts (draft_row_id, login, type_string) counts
                with
                | Error () -> Error Inconsistent_data
                | Ok view_summary ->
                    Ok
                      (Some { summary = view_summary; repositories = validated })
                )))
