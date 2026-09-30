(* Moderator-authorized, read-only view behind the pending project-home
   review queue for one target community. Authorization happens inside the
   SQL itself — the reviewer must be a current top moderator of the target
   community or a durable global administrator, exactly the durable model
   the transactional Project_home_review_store enforces — and every
   unauthorized or missing-community state collapses to the same absent
   result, so slug probing cannot become an authorization oracle.

   Three small queries on one connection, no locks: an informational GET has
   nothing to serialize — a request may be reviewed or its project's
   verification may drift after the read, and the transactional review store
   remains authoritative and independently reauthorizes and locks every
   durable row on the later POST. Postgres is trusted as the persistence
   boundary, but every durable value the page relies on is re-checked
   through the closed conversions and structural rules before a view is
   returned. See the .mli for the full contract. *)

open Lwt.Infix
module Int64_set = Set.Make (Int64)

type project_verification = Verified | Stale | Revoked
type host_eligibility = Eligible | Currently_ineligible

type error =
  | Invalid_user_id
  | Invalid_community_slug
  | Inconsistent_data
  | Storage_error

type repository = {
  repository_full_name : string;
  repository_html_url : string;
  repository_is_primary : bool;
  repository_is_archived : bool;
}

type pending_request = {
  project_name : string;
  project_slug : string;
  project_kind : Project_identity.kind;
  project_namespace_login : string;
  project_verification : project_verification;
  project_repositories : repository list;
  requester_name : string option;
  request_note : string option;
}

type community = {
  (* The local community row id is constructor context only: it scopes the
     pending-request query and is deliberately never exposed. *)
  community_row_id : int;
  community_name : string;
  community_slug : string;
  community_host_eligibility : host_eligibility;
}

type view = {
  view_community : community;
  view_pending_requests : pending_request list;
}

let community (v : view) = v.view_community
let pending_requests (v : view) = v.view_pending_requests
let community_name (c : community) = c.community_name
let community_slug (c : community) = c.community_slug
let community_host_eligibility (c : community) = c.community_host_eligibility
let project_name (r : pending_request) = r.project_name
let project_slug (r : pending_request) = r.project_slug
let project_kind (r : pending_request) = r.project_kind
let project_namespace_login (r : pending_request) = r.project_namespace_login
let project_verification (r : pending_request) = r.project_verification
let project_repositories (r : pending_request) = r.project_repositories
let requester_name (r : pending_request) = r.requester_name
let request_note (r : pending_request) = r.request_note
let repository_full_name (r : repository) = r.repository_full_name
let repository_html_url (r : repository) = r.repository_html_url
let repository_is_primary (r : repository) = r.repository_is_primary
let repository_is_archived (r : repository) = r.repository_is_archived

(* The finalization store copies at most one complete draft snapshot, and
   the GitHub client caps a listing at twenty 100-entry pages. *)
let repository_limit = 2000

(* === shared byte-level predicates === *)

(* The permanent canonical project slug shape — the same grammar
   Project_identity persists and the review store requires on routes. Route
   values are byte-preserved: nothing lowercases, trims, or repairs. *)
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

(* A single non-empty URL path segment — no whitespace, controls, DEL, or
   '/'. Community slugs, verified namespace logins, and requester usernames
   are all addressed this way; same rule the request/review stores impose on
   the community slug (the strongest community-addressability predicate the
   repository has). *)
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

(* Repository full-name halves are single path segments; branches are opaque
   but may contain '/', so only whitespace, controls, and DEL are barred —
   the same byte rules the draft/setup read models enforce on the values the
   finalization store copied. *)
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

let verification_of_string = function
  | "verified" -> Some Verified
  | "stale" -> Some Stale
  | "revoked" -> Some Revoked
  | _ -> None

(* === community lifecycle (identical semantics to Project_home_review_store) *)

(* Current-eligibility predicate for acceptance — a published, fully public
   network community. Public and unlisted both satisfy it (unlisted only
   clears the indexing/discovery flags, still visibility = 'public'). *)
let currently_eligible ~is_network_community ~onboarding_state ~visibility =
  is_network_community
  && onboarding_state = Community_types.Community_published
  && visibility = Community_types.Community_public

(* Structural validity of the target community. The shared lifecycle rule
   decides most shapes; the one drifted shape it rejects but review must
   still handle — a published network community since gone fully private —
   is valid-but-ineligible here, so pending requests can still be rejected.
   Leaking shapes (an indexable draft, a discoverable private community,
   mixed published flags) remain corruption. *)
let community_structurally_valid ~is_network_community ~onboarding_state
    ~visibility ~indexable ~discoverable =
  Network_communities.lifecycle_state_valid ~is_network_community
    ~onboarding_state ~visibility ~indexable ~discoverable
  || is_network_community
     && onboarding_state = Community_types.Community_published
     && visibility = Community_types.Community_private
     && (not indexable) && not discoverable

(* === queries === *)

(* Authorization and the target community in one statement: the community
   row returns only when the reviewer is a current top moderator of it or a
   durable global administrator. No lifecycle filter — a pending request may
   predate a lifecycle change and must still be rejectable — so the
   lifecycle columns come back for closed validation and the eligibility
   decision. A missing community and an unauthorized reviewer are
   indistinguishable: both yield zero rows. *)
let community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int)
  ->? Caqti_type.(t2 (t4 int string string string) (t4 string bool bool bool)))
    "SELECT c.id, c.slug, c.name, c.visibility, c.onboarding_state, \
     c.is_network_community, c.indexable, c.discoverable FROM communities c \
     WHERE c.slug = $1 AND (EXISTS (SELECT 1 FROM community_moderators m WHERE \
     m.user_id = $2 AND m.community_id = c.id AND m.role = 'top_mod') OR \
     EXISTS (SELECT 1 FROM users u WHERE u.id = $2 AND u.is_admin))"

(* The pending home requests for the authorized community, joined to
   permanent project data, the requester's public username (LEFT JOIN: a
   NULL provenance or deleted requester yields a NULL username), and every
   permanent repository (LEFT JOIN: a corrupt zero-repository project yields
   one all-NULL repository half, distinguishable from a project with rows).
   No verification filter — stale and revoked requests must remain
   rejectable. One statement bounds repository loading independently of the
   request count. The idx_community_projects_community_status index serves
   the (community_id, status) lookup; ORDER BY keeps each relation's rows
   contiguous (a project has one active home, so slug identifies a relation)
   and fixes the deterministic queue order plus repository position order. *)
let pending_requests_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
  ->* Caqti_type.(
        t2
          (t2
             (t4 int64 string string string)
             (t4 string string (option string) (option string)))
          (option (t2 (t4 int string string string) (t2 bool bool)))))
    "SELECT p.id, p.name, p.slug, p.kind, p.forge_namespace_login, \
     project_github_verification(p.id, p.verification_status), \
     cp.request_note, u.username, r.position, r.full_name, r.html_url, \
     r.default_branch, r.is_primary, r.is_archived FROM community_projects cp \
     JOIN open_source_projects p ON p.id = cp.project_id LEFT JOIN users u ON \
     u.id = cp.requested_by_user_id LEFT JOIN project_repositories r ON \
     r.project_id = p.id WHERE cp.community_id = $1 AND cp.relation_type = \
     'home' AND cp.status = 'pending' ORDER BY cp.created_at ASC, p.slug ASC, \
     r.position ASC"

(* === community mapping === *)

let community_of_row (id, slug, name, visibility_raw)
    (onboarding_raw, is_network_community, indexable, discoverable) =
  if not (id > 0 && nonblank name && single_path_segment slug) then Error ()
  else
    match
      ( Community_types.community_visibility_of_string visibility_raw,
        Community_types.community_onboarding_state_of_string onboarding_raw )
    with
    | Some visibility, Ok onboarding_state ->
        if
          not
            (community_structurally_valid ~is_network_community
               ~onboarding_state ~visibility ~indexable ~discoverable)
        then Error ()
        else
          let host_eligibility =
            if
              currently_eligible ~is_network_community ~onboarding_state
                ~visibility
            then Eligible
            else Currently_ineligible
          in
          Ok
            {
              community_row_id = id;
              community_name = name;
              community_slug = slug;
              community_host_eligibility = host_eligibility;
            }
    | None, _ | _, Error _ -> Error ()

(* === repository validation (per-row and cross-row) === *)

(* Position kept alongside the exposed fields for the cross-row contiguity
   and duplicate rules; dropped from the returned value. *)
type validated_repo = { vr_position : int; vr_repository : repository }

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

(* === requester and note === *)

let requester_of_row = function
  | None -> Ok None
  | Some username ->
      (* A deleted or provenance-less requester is NULL (handled above), so
         a present username is durable data — a malformed one is corruption,
         not silently dropped. *)
      if single_path_segment username then Ok (Some username) else Error ()

(* === grouping === *)

(* Rows for one relation are contiguous (see the query comment). Group by
   project id, closing a group when the id changes; a project id seen in a
   second, non-adjacent group is corruption (the one-active-home index
   forbids two active relations per project). *)
let group_by_project rows =
  let close acc = function
    | None -> acc
    | Some (_id, identity, repos_rev) -> (identity, List.rev repos_rev) :: acc
  in
  let rec go seen acc current = function
    | [] -> Ok (List.rev (close acc current))
    | (identity, repo_opt) :: rest -> (
        let (pid, _, _, _), _ = identity in
        match current with
        | Some (cur_id, cur_identity, repos_rev) when Int64.equal cur_id pid ->
            go seen acc
              (Some (cur_id, cur_identity, repo_opt :: repos_rev))
              rest
        | _ ->
            let acc = close acc current in
            if Int64_set.mem pid seen then Error ()
            else
              go (Int64_set.add pid seen) acc
                (Some (pid, identity, [ repo_opt ]))
                rest)
  in
  go Int64_set.empty [] None rows

let pending_of_group
    ( ( (project_id, name, slug, kind_raw),
        (login, verification_raw, note_raw, requester_raw) ),
      repo_opts ) =
  match
    ( Project_identity.kind_of_string kind_raw,
      verification_of_string verification_raw )
  with
  | Some kind, Some verification -> (
      if
        not
          (Int64.compare project_id 0L > 0
          && canonical_project_slug slug
          && single_path_segment login)
      then Error ()
      else
        (* The note must reconstruct byte-exactly through the pure
           constructor — a padded or control-bearing durable note is
           corruption. *)
        match Project_home_relation.create_pending ~request_note:note_raw with
        | Error _ -> Error ()
        | Ok relation -> (
            if Project_home_relation.request_note relation <> note_raw then
              Error ()
            else
              match requester_of_row requester_raw with
              | Error () -> Error ()
              | Ok requester -> (
                  if
                    (* An all-NULL repository half means the LEFT JOIN found no
                     permanent repository rows: a verified/stale/revoked
                     project must still have at least one, so a missing set
                     is corruption, never an empty request. *)
                    List.exists Option.is_none repo_opts
                  then Error ()
                  else
                    match
                      validate_repositories ~kind
                        (List.filter_map Fun.id repo_opts)
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
                            project_repositories = repositories;
                            requester_name = requester;
                            request_note = note_raw;
                          })))
  | _ -> Error ()

let rec convert_groups acc = function
  | [] -> Ok (List.rev acc)
  | group :: rest -> (
      match pending_of_group group with
      | Error () -> Error ()
      | Ok request -> convert_groups (request :: acc) rest)

let load_for_reviewer (module C : Caqti_lwt.CONNECTION) ~reviewer_user_id
    ~community_slug =
  if reviewer_user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (single_path_segment community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* As in the sibling read models, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt community_query (community_slug, reviewer_user_id) >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None ->
        (* Missing community and unauthorized reviewer collapse into the
           same absence, so probing cannot become an authorization oracle. *)
        Lwt.return (Ok None)
    | Ok (Some (identity, lifecycle)) -> (
        match community_of_row identity lifecycle with
        | Error () -> Lwt.return (Error Inconsistent_data)
        | Ok loaded_community -> (
            C.collect_list pending_requests_query
              loaded_community.community_row_id
            >|= function
            | Error _ -> Error Storage_error
            | Ok rows -> (
                match group_by_project rows with
                | Error () -> Error Inconsistent_data
                | Ok groups -> (
                    match convert_groups [] groups with
                    | Error () -> Error Inconsistent_data
                    | Ok requests ->
                        Ok
                          (Some
                             {
                               view_community = loaded_community;
                               view_pending_requests = requests;
                             })))))
