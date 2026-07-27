(* Publisher-authorized, read-only view behind the final setup surface of one
   provisioned network community. Authorization and lifecycle happen inside
   the SQL itself — slug, the network marker, the complete draft state, and
   the two durable authorization sources together in one WHERE clause, never
   a load-then-check — and every unavailable state collapses to the same
   absent result, so slug probing cannot become an authorization or lifecycle
   oracle.

   Three bounded statements, no locks: an informational GET has nothing to
   serialize, and the future atomic publication transaction remains
   authoritative. Postgres is trusted as the persistence boundary, but every
   durable value the page depends on is re-checked before a row is returned —
   the page prefills a form whose values become the community's published
   identity, so a corrupt draft must not seed one. See the .mli for the full
   contract. *)

open Lwt.Infix

type community = {
  community_name : string;
  community_slug : string;
  community_description : string option;
}

type project = {
  project_name : string;
  project_slug : string;
  project_namespace_login : string;
  project_kind : Project_identity.kind;
}

type view = {
  view_community : community;
  view_project : project;
  view_is_top_moderator : bool;
  view_is_durable_admin : bool;
}

type error =
  | Invalid_user_id
  | Invalid_community_slug
  | Inconsistent_data
  | Storage_error

let community (v : view) = v.view_community
let project (v : view) = v.view_project
let publisher_is_top_moderator (v : view) = v.view_is_top_moderator
let publisher_is_durable_admin (v : view) = v.view_is_durable_admin
let community_name (c : community) = c.community_name
let community_slug (c : community) = c.community_slug
let community_description (c : community) = c.community_description
let project_name (p : project) = p.project_name
let project_slug (p : project) = p.project_slug
let project_namespace_login (p : project) = p.project_namespace_login
let project_kind (p : project) = p.project_kind

(* === shared byte-level predicates === *)

(* A single non-empty URL path segment — no whitespace, controls, DEL, or
   '/'. The strongest community-addressability predicate in production, the
   same one the project-home request, review, and removal surfaces impose on
   a route community slug. Deliberately weaker than the canonical network
   grammar below: the route value is validated for addressability only, and
   the durable slug is what must be canonical. *)
let single_path_segment value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* The scoped network-community slug grammar, byte-for-byte the
   communities_network_slug_check CHECK: lowercase alphanumeric runs joined
   by single hyphens, at most 80 characters. ASCII-only by construction, so
   byte length is character length. *)
let canonical_network_slug value =
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

(* char_length counts Unicode scalars, never bytes — the same count
   PostgreSQL's char_length() applies in the scoped CHECKs. Only ever called
   after UTF-8 validity is established, so every decode is exact. *)
let utf8_scalar_count s =
  let n = String.length s in
  let rec go i count =
    if i >= n then count
    else
      let decode = String.get_utf_8_uchar s i in
      go (i + Uchar.utf_decode_length decode) (count + 1)
  in
  go 0 0

(* communities_network_name_check: 1..120 characters, no ASCII control byte
   and no DEL anywhere, and no space at either edge. PostgreSQL text cannot
   hold NUL, so the class effectively starts at \x01; checking from \x00 here
   is strictly safer for a value that reached OCaml. UTF-8 validity is added
   on top — the database guarantees encoding validity, and a driver-level
   surprise must not become a rendered value. *)
let valid_network_name value =
  let n = String.length value in
  n > 0
  && String.is_valid_utf_8 value
  && (not (String.exists (fun c -> c < '\x20' || c = '\x7f') value))
  && value.[0] <> ' '
  && value.[n - 1] <> ' '
  && utf8_scalar_count value <= 120

(* communities_network_description_check: NULL, or 1..2000 characters where
   LF and horizontal tab are the only permitted control bytes, with no space,
   tab, or LF at either edge. The canonical form collapses an empty
   description to NULL, so '' is corruption rather than "no description". *)
let valid_network_description = function
  | None -> true
  | Some text ->
      let n = String.length text in
      let is_forbidden c = (c < '\x20' && c <> '\n' && c <> '\t') || c = '\x7f' in
      let is_edge c = c = ' ' || c = '\t' || c = '\n' in
      n > 0
      && String.is_valid_utf_8 text
      && (not (String.exists is_forbidden text))
      && (not (is_edge text.[0]))
      && (not (is_edge text.[n - 1]))
      && utf8_scalar_count text <= 2000

(* The permanent canonical project slug shape — the same grammar
   Project_identity persists and every sibling project-home module requires.
   Checked independently of the community grammar above so the two can
   diverge later without one silently validating the other. *)
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

(* Nonblank display text: at least one byte outside ASCII whitespace. *)
let nonblank value =
  String.exists
    (fun c ->
      not (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c' || c = '\x0b'))
    value

(* The closed verification vocabulary of a permanent project. Validated as a
   known value and then discarded: see the .mli for why drift does not gate
   this view. *)
let known_verification = function
  | "verified" | "stale" | "revoked" -> true
  | _ -> false

(* === queries === *)

(* Authorization, lifecycle, and identity in one statement.

   The lifecycle predicate is spelled out positively rather than by excluding
   published: a community must be exactly a private, non-indexable,
   non-discoverable network draft to be loadable, so any future onboarding
   state is inert here until it is deliberately classified.

   The two authorization sources are returned as well as filtered, so the
   page can say which durable role is being exercised without a second query.
   A missing community, a legacy community, a published network community,
   and an unauthorized user are all indistinguishable: every one yields zero
   rows. *)
let community_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int)
   ->? Caqti_type.(
         t2
           (t4 int string string (option string))
           (t2 (t3 string string bool) (t4 bool bool bool bool))))
    "SELECT c.id, c.slug, c.name, c.description, \
            c.visibility, c.onboarding_state, c.is_network_community, \
            c.indexable, c.discoverable, \
            EXISTS (SELECT 1 FROM community_moderators m \
                    WHERE m.user_id = $2 AND m.community_id = c.id \
                      AND m.role = 'top_mod'), \
            EXISTS (SELECT 1 FROM users u WHERE u.id = $2 AND u.is_admin) \
     FROM communities c \
     WHERE c.slug = $1 \
       AND c.is_network_community = TRUE \
       AND c.onboarding_state = 'draft' \
       AND c.visibility = 'private' \
       AND c.indexable = FALSE \
       AND c.discoverable = FALSE \
       AND (EXISTS (SELECT 1 FROM community_moderators m \
                    WHERE m.user_id = $2 AND m.community_id = c.id \
                      AND m.role = 'top_mod') \
            OR EXISTS (SELECT 1 FROM users u \
                       WHERE u.id = $2 AND u.is_admin))"

(* Every accepted home relation of the authorized community, joined to
   permanent project data. The join drops a relation whose project row is
   gone, so a deleted project reads as "no accepted project" — corruption
   either way. No LIMIT: the duplicate case must be observable, not silently
   truncated to the first row. The idx_community_projects_community_status
   index serves the (community_id, status) lookup. *)
let accepted_project_query =
  let open Caqti_request.Infix in
  (Caqti_type.int
   ->* Caqti_type.(t2 (t3 int64 string string) (t3 string string string)))
    "SELECT p.id, p.name, p.slug, \
            p.kind, p.forge_namespace_login, p.verification_status \
     FROM community_projects cp \
     JOIN open_source_projects p ON p.id = cp.project_id \
     WHERE cp.community_id = $1 \
       AND cp.relation_type = 'home' \
       AND cp.status = 'accepted' \
     ORDER BY p.id"

(* The complete-draft shape in one bounded aggregate read: the community must
   still have a member and a top moderator, exactly the two shell rows the
   provisioning store created, exactly one accepted home relation and no
   other active one on the community side, and exactly one active home
   relation on the linked project's side. Counting rather than fetching keeps
   this O(1) in rows regardless of community size — no N+1. *)
let draft_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 int int64)
   ->! Caqti_type.(t2 (t4 int int int int) (t3 int int int)))
    "SELECT \
       (SELECT COUNT(*) FROM community_members WHERE community_id = $1), \
       (SELECT COUNT(*) FROM community_moderators \
        WHERE community_id = $1 AND role = 'top_mod'), \
       (SELECT COUNT(*) FROM community_sections \
        WHERE community_id = $1 AND slug = 'general'), \
       (SELECT COUNT(*) FROM channels \
        WHERE community_id = $1 AND slug = 'general' AND NOT is_archived), \
       (SELECT COUNT(*) FROM community_projects \
        WHERE community_id = $1 AND relation_type = 'home' \
          AND status = 'accepted'), \
       (SELECT COUNT(*) FROM community_projects \
        WHERE community_id = $1 AND relation_type = 'home' \
          AND status IN ('pending', 'accepted')), \
       (SELECT COUNT(*) FROM community_projects \
        WHERE project_id = $2 AND relation_type = 'home' \
          AND status IN ('pending', 'accepted'))"

(* === row mapping === *)

let community_of_row supplied_slug (id, stored_slug, name, description)
    ((visibility_raw, onboarding_raw, is_network), (indexable, discoverable, is_top_mod, is_admin))
    =
  match
    ( Db.community_visibility_of_string visibility_raw,
      Db.community_onboarding_state_of_string onboarding_raw )
  with
  | Some visibility, Ok onboarding_state ->
      if
        not
          (id > 0
          && String.equal stored_slug supplied_slug
          && canonical_network_slug stored_slug
          && valid_network_name name
          && valid_network_description description
          && is_network
          && visibility = Db.Community_private
          && onboarding_state = Db.Community_draft
          && (not indexable)
          && (not discoverable)
          (* The whole-state invariant, re-asserted through the frozen
             domain rather than restated: a leaking draft is corruption. *)
          && Network_communities.lifecycle_state_valid
               ~is_network_community:is_network ~onboarding_state ~visibility
               ~indexable ~discoverable
          (* The SQL already filtered on this disjunction; a row that
             reached here without either flag would mean the filter and the
             projection disagree. *)
          && (is_top_mod || is_admin))
      then Error ()
      else
        Ok
          ( id,
            { community_name = name;
              community_slug = stored_slug;
              community_description = description
            },
            is_top_mod,
            is_admin )
  | None, _ | _, Error _ -> Error ()

let project_of_row ((row_id, name, slug), (kind_raw, login, verification)) =
  match Project_identity.kind_of_string kind_raw with
  | None -> Error ()
  | Some kind ->
      if
        not
          (Int64.compare row_id 0L > 0
          && canonical_project_slug slug
          && nonblank name
          && single_path_segment login
          && known_verification verification)
      then Error ()
      else
        Ok
          ( row_id,
            { project_name = name;
              project_slug = slug;
              project_namespace_login = login;
              project_kind = kind
            } )

let load_for_publisher (module C : Caqti_lwt.CONNECTION) ~user_id
    ~community_slug =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (single_path_segment community_slug) then
    Lwt.return (Error Invalid_community_slug)
  else
    (* As in the sibling read models, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt community_query (community_slug, user_id) >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None ->
        (* Missing, legacy, published, and unauthorized collapse into the
           same absence. *)
        Lwt.return (Ok None)
    | Ok (Some (identity, lifecycle)) -> (
        match community_of_row community_slug identity lifecycle with
        | Error () -> Lwt.return (Error Inconsistent_data)
        | Ok (community_row_id, loaded_community, is_top_mod, is_admin) -> (
            C.collect_list accepted_project_query community_row_id >>= function
            | Error _ -> Lwt.return (Error Storage_error)
            (* Zero accepted home projects (including a pending-only
               relation or a deleted project row) and more than one are both
               corruption: a provisioned draft has exactly one home. *)
            | Ok ([] | _ :: _ :: _) -> Lwt.return (Error Inconsistent_data)
            | Ok [ row ] -> (
                match project_of_row row with
                | Error () -> Lwt.return (Error Inconsistent_data)
                | Ok (project_row_id, loaded_project) -> (
                    C.find draft_state_query (community_row_id, project_row_id)
                    >|= function
                    | Error _ -> Error Storage_error
                    | Ok
                        ( (members, top_mods, sections, channels),
                          (accepted, community_active, project_active) ) ->
                        if
                          not
                            (members >= 1 && top_mods >= 1 && sections = 1
                           && channels = 1 && accepted = 1
                           && community_active = 1 && project_active = 1)
                        then Error Inconsistent_data
                        else
                          Ok
                            (Some
                               {
                                 view_community = loaded_community;
                                 view_project = loaded_project;
                                 view_is_top_moderator = is_top_mod;
                                 view_is_durable_admin = is_admin;
                               })))))
