(* Owner-authorized, read-only view behind the existing-community home
   choice for one permanent verified project. Authorization happens inside
   the SQL itself — slug, stewardship, and verified status together in one
   WHERE clause, never a load-then-check — and every unavailable state
   collapses to the same absent result, so slug probing cannot become an
   ownership or state oracle.

   Three small queries on one connection, no locks: an informational GET
   has nothing to serialize — a concurrent request may be created after the
   read, and the transactional request store remains authoritative
   (Active_home_exists). Postgres is trusted as the persistence boundary,
   but every durable value the page relies on is re-checked through the
   closed conversions before a row is returned. See the .mli for the full
   contract. *)

open Lwt.Infix

type visibility =
  | Public
  | Unlisted
  | Currently_unavailable

type project = {
  project_name : string;
  project_slug : string;
  project_namespace_login : string;
}

type community = {
  community_id : int;
  community_name : string;
  community_slug : string;
  community_description : string option;
  community_visibility : visibility;
}

type active_relation = {
  relation_status : Project_home_relation.status;
  relation_community : community;
}

type view = {
  view_project : project;
  view_active_relation : active_relation option;
  view_eligible_communities : community list;
}

type error =
  | Invalid_user_id
  | Invalid_project_slug
  | Inconsistent_data
  | Storage_error

let project_name (p : project) = p.project_name
let project_slug (p : project) = p.project_slug
let project_namespace_login (p : project) = p.project_namespace_login
let community_id (c : community) = c.community_id
let community_name (c : community) = c.community_name
let community_slug (c : community) = c.community_slug
let community_description (c : community) = c.community_description
let community_visibility (c : community) = c.community_visibility
let active_relation_status (r : active_relation) = r.relation_status
let active_relation_community (r : active_relation) = r.relation_community
let project (v : view) = v.view_project
let active_relation (v : view) = v.view_active_relation
let eligible_communities (v : view) = v.view_eligible_communities

(* The permanent canonical slug shape — the same grammar Project_identity
   persists and the sibling setup read model requires on routes. Route
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

(* A single non-empty URL path segment — no whitespace, controls, DEL, or
   '/'. Every community is addressed at /c/:slug and every verified
   namespace login rides in GitHub URLs, so both share this shape; same
   rule as the transactional request store. *)
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
      not (c = ' ' || c = '\t' || c = '\r' || c = '\n' || c = '\x0c'
           || c = '\x0b'))
    value

(* Community descriptions are multi-line free text: LF and horizontal tab
   survive as content; every other ASCII control byte and DEL is durable
   corruption. *)
let control_safe_description value =
  String.for_all
    (fun c -> (Char.code c >= 0x20 && c <> '\x7f') || c = '\n' || c = '\t')
    value

let valid_description = function
  | None -> true
  | Some text -> control_safe_description text

(* === queries === *)

(* Authorization in one statement: stewardship (the steward primary key
   (project_id, user_id) caps the join at one row), canonical slug, and
   verified status together. Creator and installation provenance are never
   consulted. verification_status comes back for closed-value revalidation,
   not filtering. *)
let project_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int)
   ->? Caqti_type.(t2 (t2 int64 string) (t3 string string string)))
    "SELECT p.id, p.name, p.slug, p.forge_namespace_login, \
            p.verification_status \
     FROM open_source_projects p \
     JOIN project_stewards s ON s.project_id = p.id AND s.user_id = $2 \
     WHERE p.slug = $1 AND p.verification_status = 'verified'"

(* The project's active home relation with its target community identity.
   Deliberately no eligibility predicate on the community side: a pending
   or accepted relation is historical workflow state, and the target must
   stay visible even after its lifecycle later became ineligible. The
   partial unique active-home index guarantees at most one row; observing
   more is corruption. *)
let active_relation_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64
   ->* Caqti_type.(
         t2
           (t2 (t2 string int) (t2 string string))
           (t2
              (t2 (option string) string)
              (t2 (t2 string bool) (t2 bool bool)))))
    "SELECT cp.status, c.id, c.name, c.slug, c.description, c.visibility, \
            c.onboarding_state, c.is_network_community, \
            c.indexable, c.discoverable \
     FROM community_projects cp \
     JOIN communities c ON c.id = cp.community_id \
     WHERE cp.project_id = $1 \
       AND cp.relation_type = 'home' \
       AND cp.status IN ('pending', 'accepted')"

(* Every currently eligible target, in the exact current durable model: a
   network community, live, and not fully private. Legacy communities and
   network setup drafts fall out of the same predicate. The lifecycle
   columns come back for closed-value validation, not filtering. Ordering
   is deterministic and content-based — no member, activity, or popularity
   ranking exists. *)
let eligible_communities_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit
   ->* Caqti_type.(
         t2
           (t2 (t2 int string) (t2 string (option string)))
           (t2 (t2 string string) (t2 bool bool))))
    "SELECT id, name, slug, description, visibility, onboarding_state, \
            indexable, discoverable \
     FROM communities \
     WHERE is_network_community \
       AND onboarding_state = 'published' \
       AND visibility = 'public' \
     ORDER BY lower(name) ASC, slug ASC, id ASC"

(* === row validation === *)

(* Shared identity rules for any returned community row. Errors are
   deliberately unit: which rule failed on which value must not travel. *)
let community_identity_valid ~id ~name ~slug ~description =
  id > 0 && nonblank name
  && single_path_segment slug
  && valid_description description

(* The active target's presentation visibility over its complete current
   lifecycle. Only the two exact eligible published shapes earn their
   labels — Public (fully listed) and Unlisted (fully unlisted) mean
   precisely those states and nothing else. Every individually valid but
   no-longer-eligible lifecycle (legacy/non-network, setup draft, private,
   and valid combinations of those) collapses to Currently_unavailable
   without revealing which condition applies: the relation is historical
   workflow state and must stay visible, but never under a false label.

   Unavailability is not corruption: an unknown enum value or a mixed
   indexable/discoverable pair contradicts the durable model itself and
   is Inconsistent_data — deliberately NOT the full lifecycle_state_valid
   rule, whose published-must-be-public clause would misread an ordinary
   drift to private as corruption. *)
let active_target_visibility ~visibility_raw ~onboarding_raw
    ~is_network_community ~indexable ~discoverable =
  match
    ( Db.community_visibility_of_string visibility_raw,
      Db.community_onboarding_state_of_string onboarding_raw )
  with
  | Some parsed_visibility, Ok parsed_onboarding ->
      if indexable <> discoverable then Error ()
      else if
        is_network_community
        && parsed_onboarding = Db.Community_published
        && parsed_visibility = Db.Community_public
      then Ok (if indexable then Public else Unlisted)
      else Ok Currently_unavailable
  | None, _ | _, Error _ -> Error ()

let active_community_of_row ((status_raw, id), (name, slug))
    ( (description, visibility_raw),
      ((onboarding_raw, is_network_community), (indexable, discoverable)) ) =
  match Project_home_relation.status_of_string status_raw with
  | Some ((Project_home_relation.Pending | Project_home_relation.Accepted) as
          status) ->
      if not (community_identity_valid ~id ~name ~slug ~description) then
        Error ()
      else (
        match
          active_target_visibility ~visibility_raw ~onboarding_raw
            ~is_network_community ~indexable ~discoverable
        with
        | Error () -> Error ()
        | Ok visibility ->
            Ok
              {
                relation_status = status;
                relation_community =
                  {
                    community_id = id;
                    community_name = name;
                    community_slug = slug;
                    community_description = description;
                    community_visibility = visibility;
                  };
              })
  | Some _ | None -> Error ()

(* An eligible row must be exactly one of the two published shapes the
   shared lifecycle rule permits — fully listed (Public) or fully unlisted
   (Unlisted). A mixed flag pair is corruption, not a third mode. *)
let eligible_community_of_row
    (((id, name), (slug, description)),
     ((visibility_raw, onboarding_raw), (indexable, discoverable))) =
  if not (community_identity_valid ~id ~name ~slug ~description) then
    Error ()
  else
    match
      ( Db.community_visibility_of_string visibility_raw,
        Db.community_onboarding_state_of_string onboarding_raw )
    with
    | Some parsed_visibility, Ok parsed_onboarding ->
        if
          not
            (Network_communities.lifecycle_state_valid
               ~is_network_community:true
               ~onboarding_state:parsed_onboarding
               ~visibility:parsed_visibility ~indexable ~discoverable)
        then Error ()
        else
          Ok
            {
              community_id = id;
              community_name = name;
              community_slug = slug;
              community_description = description;
              community_visibility = (if indexable then Public else Unlisted);
            }
    | None, _ | _, Error _ -> Error ()

let rec convert_eligible acc = function
  | [] -> Ok (List.rev acc)
  | row :: rest -> (
      match eligible_community_of_row row with
      | Error () -> Error ()
      | Ok community -> convert_eligible (community :: acc) rest)

let load_for_steward (module C : Caqti_lwt.CONNECTION) ~user_id ~project_slug
    =
  if user_id <= 0 then Lwt.return (Error Invalid_user_id)
  else if not (canonical_project_slug project_slug) then
    Lwt.return (Error Invalid_project_slug)
  else
    (* As in the sibling read models, every Caqti error is dropped
       payload-free — error payloads can echo SQL parameters. *)
    C.find_opt project_query (project_slug, user_id) >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok None ->
        (* Missing project, another user's, creator without stewardship,
           stale, and revoked all collapse into the same absence. *)
        Lwt.return (Ok None)
    | Ok (Some ((row_id, stored_name), (stored_slug, login, verification)))
      ->
        if
          not
            (Int64.compare row_id 0L > 0
            && String.equal stored_slug project_slug
            && canonical_project_slug stored_slug
            && single_path_segment login
            && String.equal verification "verified")
        then Lwt.return (Error Inconsistent_data)
        else
          let loaded_project =
            {
              project_name = stored_name;
              project_slug = stored_slug;
              project_namespace_login = login;
            }
          in
          C.collect_list active_relation_query row_id >>= ( function
          | Error _ -> Lwt.return (Error Storage_error)
          | Ok (_ :: _ :: _) ->
              (* The partial unique active-home index promises at most one
                 active row; a second one is corruption. *)
              Lwt.return (Error Inconsistent_data)
          | Ok [ (identity, detail) ] -> (
              match active_community_of_row identity detail with
              | Error () -> Lwt.return (Error Inconsistent_data)
              | Ok relation ->
                  (* An active relation closes the choice: no eligible list
                     is offered, so the page cannot solicit another
                     request. *)
                  Lwt.return
                    (Ok
                       (Some
                          {
                            view_project = loaded_project;
                            view_active_relation = Some relation;
                            view_eligible_communities = [];
                          })))
          | Ok [] -> (
              C.collect_list eligible_communities_query () >|= function
              | Error _ -> Error Storage_error
              | Ok rows -> (
                  match convert_eligible [] rows with
                  | Error () -> Error Inconsistent_data
                  | Ok communities ->
                      Ok
                        (Some
                           {
                             view_project = loaded_project;
                             view_active_relation = None;
                             view_eligible_communities = communities;
                           }))) )
