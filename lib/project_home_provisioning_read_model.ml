(* Owner-authorized, read-only view behind the dedicated-community-home
   creation entry for one permanent verified project. Authorization happens
   inside the SQL itself — slug, stewardship, verified status, and the
   active-home exclusion together in one WHERE clause, never a load-then-check
   — and every unavailable state collapses to the same absent result, so slug
   probing cannot become an ownership or state oracle.

   One statement, no locks: an informational GET has nothing to serialize.
   A concurrent request or provisioning may create an active relation after
   the read, and the future transactional provisioning store remains
   authoritative. Postgres is trusted as the persistence boundary, but every
   durable value the suggestion depends on is re-checked before a row is
   returned — the page prefills a form whose values become a durable
   community identity, so a corrupt project must not seed one. See the .mli
   for the full contract. *)

open Lwt.Infix

type project = {
  project_name : string;
  project_slug : string;
  project_description : string option;
  project_kind : Project_identity.kind;
  project_namespace_login : string;
}

type view = { view_project : project; view_suggested_slug : string }

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Inconsistent_data
  | Storage_error

let project_name (p : project) = p.project_name
let project_slug (p : project) = p.project_slug
let project_description (p : project) = p.project_description
let project_kind (p : project) = p.project_kind
let project_namespace_login (p : project) = p.project_namespace_login
let project (v : view) = v.view_project

(* Suggestions are projections of the already-validated project: no second
   source of truth, and nothing derived that the project itself does not
   already guarantee. *)
let suggested_community_name (v : view) = v.view_project.project_name
let suggested_community_slug (v : view) = v.view_suggested_slug

let suggested_community_description (v : view) =
  v.view_project.project_description

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and the sibling project-home read models require on routes. Route
   values must already be canonical: nothing here lowercases, trims, or
   repairs, so an aliased spelling can never reach the database. *)
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

(* The community creation-slug policy, checked independently of the project
   grammar above so the two can diverge without silently producing a
   suggestion the form would reject. Today they coincide. *)
let community_creation_slug value =
  let n = String.length value in
  let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  n >= 1 && n <= 80
  && value.[0] <> '-'
  && value.[n - 1] <> '-'
  &&
  let rec ok i =
    i >= n
    ||
    if is_slug_char value.[i] then ok (i + 1)
    else value.[i] = '-' && value.[i + 1] <> '-' && ok (i + 1)
  in
  ok 0

(* A single non-empty URL path segment — no whitespace, controls, DEL, or
   '/'. Every verified namespace login rides in GitHub URLs; same rule as the
   sibling read models. *)
let single_path_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

let is_ascii_whitespace c =
  c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'

let trim_ascii s =
  let n = String.length s in
  let start = ref 0 in
  while !start < n && is_ascii_whitespace s.[!start] do
    incr start
  done;
  let stop = ref n in
  while !stop > !start && is_ascii_whitespace s.[!stop - 1] do
    decr stop
  done;
  String.sub s !start (!stop - !start)

let utf8_scalar_count s =
  let n = String.length s in
  let rec go i count =
    if i >= n then count
    else
      let decode = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length decode) (count + 1)
  in
  go 0 0

let is_ascii_control_or_del c = c < '\x20' || c = '\x7f'

(* The exact rules Project_identity applied before the finalization store
   persisted this name, re-asserted here: a stored name that would not be
   accepted as a community name today must not be offered as one. *)
let valid_project_name value =
  String.equal (trim_ascii value) value
  && value <> ""
  && String.is_valid_utf_8 value
  && (not (String.exists is_ascii_control_or_del value))
  && utf8_scalar_count value <= 120

(* Multi-line free text: LF and horizontal tab survive as content; every
   other ASCII control byte and DEL is durable corruption. A stored
   description is never blank — Project_identity collapses empty to NULL. *)
let valid_project_description = function
  | None -> true
  | Some text ->
      let is_forbidden c =
        is_ascii_control_or_del c && c <> '\n' && c <> '\t'
      in
      String.equal (trim_ascii text) text
      && text <> "" && String.is_valid_utf_8 text
      && (not (String.exists is_forbidden text))
      && utf8_scalar_count text <= 2000

(* Authorization in one statement: stewardship (the steward primary key
   (project_id, user_id) caps the join at one row), canonical slug, verified
   status, and the absence of an active home relation. Creator and
   installation provenance are never consulted. verification_status comes
   back for closed-value revalidation, not filtering.

   The NOT EXISTS names the two active statuses explicitly rather than
   excluding the closed ones, so a status added to the enum later is inert
   here until it is deliberately classified. *)
let load_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int)
  ->? Caqti_type.(
        t2 (t3 int64 string string) (t4 (option string) string string string)))
    "SELECT p.id, p.name, p.slug, p.description, p.kind, \
     p.forge_namespace_login, p.verification_status FROM open_source_projects \
     p JOIN project_stewards s ON s.project_id = p.id AND s.user_id = $2 WHERE \
     p.slug = $1 AND p.verification_status = 'verified' AND \
     github_evidence_is_fresh(s.github_verified_at) AND NOT EXISTS ( SELECT 1 \
     FROM community_projects cp WHERE cp.project_id = p.id AND \
     cp.relation_type = 'home' AND cp.status IN ('pending', 'accepted'))"

let load_for_steward (module C : Caqti_lwt.CONNECTION) ~user_id ~project_slug =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else
    (* As in the sibling read models, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt load_query (project_slug, user_id) >|= function
    | Error _ -> Error Storage_error
    | Ok None ->
        (* Missing project, another user's, creator without stewardship,
           stale, revoked, and a project already holding a pending or
           accepted home all collapse into the same absence. *)
        Ok None
    | Ok
        (Some
           ( (row_id, stored_name, stored_slug),
             (stored_description, kind_raw, login, verification) )) -> (
        match Project_identity.kind_of_string kind_raw with
        | None -> Error Inconsistent_data
        | Some kind ->
            if
              not
                (Int64.compare row_id 0L > 0
                && String.equal stored_slug project_slug
                && canonical_project_slug stored_slug
                && String.equal verification "verified"
                && valid_project_name stored_name
                && single_path_segment login
                && valid_project_description stored_description)
            then Error Inconsistent_data
            else
              let loaded =
                {
                  project_name = stored_name;
                  project_slug = stored_slug;
                  project_description = stored_description;
                  project_kind = kind;
                  project_namespace_login = login;
                }
              in
              (* An unusable suggestion degrades to "type one" rather than
                 to a repaired value the steward never chose. *)
              let suggested =
                if community_creation_slug stored_slug then stored_slug else ""
              in
              Ok
                (Some { view_project = loaded; view_suggested_slug = suggested })
        )
