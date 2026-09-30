(* The accepted project-home relations of one community, read for the
   "Connected projects" section of the existing community page.

   Two small queries on one connection, no locks and no transaction beyond
   the statement scope: this is an informational read on a page that is
   already rendering, and a relation may legitimately be accepted or removed
   moments after it. The transactional review store remains authoritative for
   every state change.

   This module never decides visibility. The existing community route
   resolves the community and completes Community_read_gate.can_view_community first,
   and only then reads here — so the section cannot become a side channel for
   a private or draft community. What is still owned here is durable
   identity: the slug must resolve to exactly one coherent community row, and
   every project, relation, and repository value the page will render is
   re-checked through the closed conversions and structural rules before any
   result is returned. Postgres is trusted as the persistence boundary, not as
   a validator. See the .mli for the full contract. *)

open Lwt.Infix

module Int64_set = Set.Make (Int64)

type verification =
  | Verified
  | Stale
  | Revoked

type error =
  | Invalid_community_slug
  | Community_unavailable
  | Inconsistent_data
  | Storage_error

type repository = {
  repository_full_name : string;
  repository_html_url : string;
  repository_is_primary : bool;
  repository_is_archived : bool;
}

type project = {
  project_name : string;
  project_slug : string;
  project_kind : Project_identity.kind;
  project_namespace_login : string;
  project_verification : verification;
  project_website_url : string option;
  project_repositories : repository list;
}

(* The community row id is constructor context only: it scopes the relation
   query and is deliberately never exposed. *)
type loaded_community = { community_row_id : int }

let repository_full_name (r : repository) = r.repository_full_name
let repository_html_url (r : repository) = r.repository_html_url
let repository_is_primary (r : repository) = r.repository_is_primary
let repository_is_archived (r : repository) = r.repository_is_archived
let project_name (p : project) = p.project_name
let project_slug (p : project) = p.project_slug
let project_kind (p : project) = p.project_kind
let project_namespace_login (p : project) = p.project_namespace_login
let project_verification (p : project) = p.project_verification
let project_website_url (p : project) = p.project_website_url
let project_repositories (p : project) = p.project_repositories

(* The finalization store copies at most one complete draft snapshot, and the
   GitHub client caps a listing at twenty 100-entry pages. *)
let repository_limit = 2000

(* === shared byte-level predicates === *)

(* A single non-empty URL path segment — no ASCII whitespace, controls, DEL,
   or '/'. This is the strongest community-addressability predicate the
   project-home modules already use (the request and review stores impose
   exactly it on a community slug), and the same shape verified GitHub
   namespace logins satisfy. Route values are byte-preserved: nothing trims,
   lowercases, percent-decodes, or repairs. *)
let single_path_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Nonblank display text: at least one byte outside ASCII whitespace. *)
let nonblank value =
  String.exists
    (fun c ->
      not
        (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

(* A community name is free display text, so only the byte classes that
   cannot appear in rendered text at all are barred. *)
let control_safe value =
  String.for_all
    (fun byte ->
      let code = Char.code byte in
      (code >= 0x20 && code <> 0x7f) || byte = '\t')
    value

(* The permanent canonical project slug shape — the same grammar
   Project_identity persists and the project-home routes require. *)
let canonical_project_slug value =
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

(* Repository full-name halves are single path segments; branches are opaque
   but may contain '/', so only whitespace, controls, and DEL are barred —
   the same byte rules the sibling setup and review read models enforce on
   the values the finalization store copied. *)
let valid_segment = single_path_segment

let valid_branch value =
  String.length value > 0
  && String.for_all
       (fun byte -> Char.code byte > 0x20 && Char.code byte <> 0x7f)
       value

(* Structured reconstruction, exactly as the GitHub client built the value the
   finalization store copied — never string comparison against
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

let verification_of_string = function
  | "verified" -> Some Verified
  | "stale" -> Some Stale
  | "revoked" -> Some Revoked
  | _ -> None

(* The permanent project website grammar belongs to Project_identity — the
   same constructor that accepted the value when the project was created — so
   it is reused rather than restated here. A fixed placeholder identity
   carries the constructor as far as the website step, and the accepted value
   must come back byte-identical: a stored URL that would no longer be
   accepted, or that the URI layer would re-serialize, is corruption rather
   than something to silently drop. *)
let validated_website stored =
  match stored with
  | None -> Ok None
  | Some _ -> (
      match
        Project_identity.create ~kind:Project_identity.Other ~name:"p" ~slug:"p"
          ~description:None ~website_url:stored ~selected_snapshot_ids:[ 1L ]
          ~primary_snapshot_id:None
      with
      | Error _ -> Error ()
      | Ok identity ->
          if Project_identity.website_url identity = stored then Ok stored
          else Error ())

(* === queries === *)

(* Durable community identity for the supplied slug. No lifecycle filter and
   no authorization: the caller has already decided both, and an accepted
   relation must survive the community drifting to unlisted, private, draft,
   or legacy. The lifecycle columns come back only so their closed vocabulary
   can be revalidated. *)
let community_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t2 (t3 int string string) (t2 string string)))
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state \
     FROM communities c \
     WHERE c.slug = $1"

(* The accepted home relations of the community, joined to permanent project
   data and every permanent repository (LEFT JOIN: a corrupt zero-repository
   project yields one all-NULL repository half, distinguishable from a project
   that has rows). Provenance is deliberately absent from both the projection
   and the predicate — an automatically provisioned home has no requester and
   no reviewer, and must appear exactly like a reviewed one. The private
   request note is never selected.

   The relation id is selected as private grouping context only: it proves
   one accepted relation per project even if durable corruption were to bypass
   the one-active-home partial unique index. It never leaves this module.

   One statement bounds repository loading independently of the project count.
   The idx_community_projects_community_status index serves the
   (community_id, status) lookup; ORDER BY both fixes the deterministic
   project order and keeps each project's rows contiguous and its repositories
   in stored position order. *)
let accepted_projects_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->* Caqti_type.(
         t2
           (t2
              (t4 int64 int64 string string)
              (t4 string string string (option string)))
           (option (t2 (t4 int string string string) (t2 bool bool)))))
    "SELECT cp.id, p.id, p.name, p.slug, \
            p.kind, p.forge_namespace_login, \
            project_github_verification(p.id, p.verification_status), \
            p.website_url, \
            r.position, r.full_name, r.html_url, r.default_branch, \
            r.is_primary, r.is_archived \
     FROM community_projects cp \
     JOIN open_source_projects p ON p.id = cp.project_id \
     LEFT JOIN project_repositories r ON r.project_id = p.id \
     WHERE cp.community_id = $1 \
       AND cp.relation_type = 'home' AND cp.status = 'accepted' \
     ORDER BY lower(p.name) ASC, p.slug ASC, p.id ASC, r.position ASC"

(* === community mapping === *)

(* Durable identity only. Private, draft, unlisted, and legacy states are all
   legitimate here — the caller owns visibility — so the lifecycle columns are
   checked for closed vocabulary and nothing more. *)
let community_of_row ~community_slug (id, stored_slug, name) (visibility, onboarding) =
  if
    not
      (id > 0
      && String.equal stored_slug community_slug
      && nonblank name && control_safe name)
  then Error ()
  else
    match
      ( Community_types.community_visibility_of_string visibility,
        Community_types.community_onboarding_state_of_string onboarding )
    with
    | Some _, Ok _ -> Ok { community_row_id = id }
    | None, _ | _, Error _ -> Error ()

(* === repository validation (per-row and cross-row) === *)

(* Position kept alongside the exposed fields for the cross-row contiguity and
   duplicate rules; dropped from the returned value. *)
type validated_repo = {
  vr_position : int;
  vr_repository : repository;
}

let repository_of_row
    ((position, full_name, html_url, default_branch), (is_primary, is_archived))
    =
  let valid =
    position > 0
    && (match split_full_name full_name with
       | None -> false
       | Some (owner_login, name) ->
           String.equal html_url (canonical_html_url ~owner_login ~name))
    && valid_branch default_branch
  in
  if valid then
    Ok
      {
        vr_position = position;
        vr_repository =
          {
            repository_full_name = full_name;
            repository_html_url = html_url;
            repository_is_primary = is_primary;
            repository_is_archived = is_archived;
          };
      }
  else Error ()

let all_distinct values =
  List.length (List.sort_uniq compare values) = List.length values

(* Rows arrive ordered by position, so contiguity from 1 also rules out
   duplicate positions. *)
let contiguous_positions repos =
  let rec check expected = function
    | [] -> true
    | (r : validated_repo) :: rest ->
        r.vr_position = expected && check (expected + 1) rest
  in
  check 1 repos

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
    | Ok repos ->
        let primaries =
          List.length
            (List.filter
               (fun (r : validated_repo) ->
                 r.vr_repository.repository_is_primary)
               repos)
        in
        let primary_rule =
          match kind with
          | Project_identity.Project -> primaries = 1
          | _ -> primaries <= 1
        in
        if
          contiguous_positions repos
          && all_distinct
               (List.map
                  (fun (r : validated_repo) ->
                    r.vr_repository.repository_full_name)
                  repos)
          && primary_rule
        then Ok (List.map (fun (r : validated_repo) -> r.vr_repository) repos)
        else Error ()

(* === grouping === *)

(* Rows for one project are contiguous (see the query comment). A group closes
   when the project id changes; a project id seen again in a later,
   non-adjacent group is corruption. Relation ids are accumulated so a project
   carrying more than one accepted relation is rejected even though the
   one-active-home partial unique index should make that unreachable. *)
let group_by_project rows =
  let close acc = function
    | None -> acc
    | Some (_id, identity, relation_ids, repos_rev) ->
        (identity, relation_ids, List.rev repos_rev) :: acc
  in
  let rec go seen acc current = function
    | [] -> Ok (List.rev (close acc current))
    | (identity, repo_opt) :: rest -> (
        let (relation_id, project_id, _, _), _ = identity in
        match current with
        | Some (cur_id, cur_identity, relation_ids, repos_rev)
          when Int64.equal cur_id project_id ->
            go seen acc
              (Some
                 ( cur_id,
                   cur_identity,
                   Int64_set.add relation_id relation_ids,
                   repo_opt :: repos_rev ))
              rest
        | _ ->
            let acc = close acc current in
            if Int64_set.mem project_id seen then Error ()
            else
              go
                (Int64_set.add project_id seen)
                acc
                (Some
                   ( project_id,
                     identity,
                     Int64_set.singleton relation_id,
                     [ repo_opt ] ))
                rest)
  in
  go Int64_set.empty [] None rows

let project_of_group
    ( ( (relation_id, project_id, name, slug),
        (kind_raw, login, verification_raw, website_raw) ),
      relation_ids,
      repo_opts ) =
  match
    ( Project_identity.kind_of_string kind_raw,
      verification_of_string verification_raw )
  with
  | Some kind, Some verification -> (
      if
        not
          (Int64.compare relation_id 0L > 0
          && Int64.compare project_id 0L > 0
          && Int64_set.cardinal relation_ids = 1
          && canonical_project_slug slug
          && nonblank name && control_safe name
          && single_path_segment login)
      then Error ()
      else
        match validated_website website_raw with
        | Error () -> Error ()
        | Ok website ->
            (* An all-NULL repository half means the LEFT JOIN found no
               permanent repository rows: a project with an accepted home must
               still have at least one, so a missing set is corruption, never a
               silently repository-less project. *)
            if List.exists Option.is_none repo_opts then Error ()
            else (
              match
                validate_repositories ~kind (List.filter_map Fun.id repo_opts)
              with
              | Error () -> Error ()
              | Ok repositories ->
                  Ok
                    {
                      project_name = name;
                      project_slug = slug;
                      project_kind = kind;
                      project_namespace_login = login;
                      project_verification = verification;
                      project_website_url = website;
                      project_repositories = repositories;
                    }))
  | _ -> Error ()

let rec convert_groups acc = function
  | [] -> Ok (List.rev acc)
  | group :: rest -> (
      match project_of_group group with
      | Error () -> Error ()
      | Ok project -> convert_groups (project :: acc) rest)

let load_for_community (module C : Caqti_lwt.CONNECTION) ~community_slug =
  if not (single_path_segment community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* As in the sibling read models, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt community_query community_slug >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None -> Lwt.return (Error Community_unavailable)
    | Ok (Some (identity, lifecycle)) -> (
        match community_of_row ~community_slug identity lifecycle with
        | Error () -> Lwt.return (Error Inconsistent_data)
        | Ok loaded -> (
            C.collect_list accepted_projects_query loaded.community_row_id
            >|= function
            | Error _ -> Error Storage_error
            | Ok rows -> (
                match group_by_project rows with
                | Error () -> Error Inconsistent_data
                | Ok groups -> (
                    match convert_groups [] groups with
                    | Error () -> Error Inconsistent_data
                    | Ok projects -> Ok projects))))
