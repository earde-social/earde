(* Connected projects on community pages: visits and relation toggles. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Rm = Earde.Project_home_removal_store

(* Two production constraints make their defensive read-model branches
   unreachable from data alone. Each is dropped and restored inside the one
   case that needs it, under Lwt.finalize so a failed assertion still leaves
   the schema exactly as it was; production migrations are untouched. *)
let q_drop_verification_check =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE open_source_projects DROP CONSTRAINT \
     open_source_projects_verification_status_check"

let q_add_verification_check =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE open_source_projects ADD CONSTRAINT \
     open_source_projects_verification_status_check CHECK (verification_status \
     IN ('verified', 'stale', 'revoked'))"

let q_set_admin =
  (Caqti_type.(t2 int bool) ->. Caqti_type.unit)
    "UPDATE users SET is_admin = $2 WHERE id = $1"

let q_set_verification =
  (Caqti_type.(t2 int64 string) ->. Caqti_type.unit)
    "UPDATE open_source_projects SET verification_status = $2 WHERE id = $1"

let q_corrupt_login =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE open_source_projects SET forge_namespace_login = 'ccph' || chr(1) \
     || 'bad' WHERE id = $1"

(* Hiding the relation table makes the community lookup succeed and the
   connected-projects read fail — the only way to exercise the storage
   branch of THIS feature rather than the route's pre-existing lookup
   failure. Renamed and restored inside the one case that needs it, under
   Lwt.finalize; production migrations are untouched. *)
let q_hide_relations =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE community_projects RENAME TO community_projects_ccph_hidden"

let q_show_relations =
  (Caqti_type.unit ->. Caqti_type.unit)
    "ALTER TABLE community_projects_ccph_hidden RENAME TO community_projects"

(* One shared single-connection sql_pool pipeline for the whole suite:
   nothing ever closes a Dream.sql_pool, and this suite issues enough
   requests (several visitor identities per case) that a fresh pool per
   request exhausts Postgres max_connections. The session identity is
   swapped per request instead; cases run sequentially. Everything else is
   the real production shape — secret, memory sessions, and the real
   "/c/:slug" router path bound to the real handler, so :slug,
   Community_store.get_community_by_slug and can_view_community behave exactly as in
   bin/main. *)
let shared_identity : (int * bool) option ref = ref None
let shared_pipeline = ref None

let pipeline_for ~url =
  match !shared_pipeline with
  | Some pipeline -> pipeline
  | None ->
      let pipeline =
        Dream.sql_pool ~size:1 url
        @@ Dream.set_secret Github_fixture.cookie_secret
        @@ Dream.memory_sessions
        @@ (fun handler request ->
          match !shared_identity with
          | None -> handler request
          | Some (uid, is_admin) ->
              let* () =
                Dream.set_session_field request "user_id" (string_of_int uid)
              in
              let* () =
                if is_admin then
                  Dream.set_session_field request "is_admin" "true"
                else Lwt.return_unit
              in
              handler request)
        @@ Dream.router
             [
               Dream.get "/c/:slug"
                 Earde.Community_handlers.community_page_handler;
               Dream.get "/c/:slug/network"
                 Earde.Community_handlers.community_network_handler;
             ]
      in
      shared_pipeline := Some pipeline;
      pipeline

(* The complete list moved off the structured community home to the public
   Network page, so [visit] follows it there by default: same read model,
   same route-level authorization decision, one page further along. The
   surfaces that still carry the section inline — the flat community's own
   side stack — pass [~path:""] explicitly. *)
let visit ?session_user_id ?(session_admin = false) ?(path = "/network") ~url
    ~slug () =
  let pipeline = pipeline_for ~url in
  (shared_identity :=
     match session_user_id with
     | None -> None
     | Some uid -> Some (uid, session_admin));
  let* response =
    pipeline (Dream.request ~method_:`GET ~target:("/c/" ^ slug ^ path) "")
  in
  let* body = Dream.body response in
  Lwt.return (response, body)

let error_str : Rm.error -> string = function
  | Rm.Invalid_user_id -> "Invalid_user_id"
  | Rm.Invalid_project_slug -> "Invalid_project_slug"
  | Rm.Invalid_community_slug -> "Invalid_community_slug"
  | Rm.Project_unavailable -> "Project_unavailable"
  | Rm.Community_unavailable -> "Community_unavailable"
  | Rm.Actor_unauthorized -> "Actor_unauthorized"
  | Rm.Removal_unavailable -> "Removal_unavailable"
  | Rm.Inconsistent_data -> "Inconsistent_data"
  | Rm.Storage_error -> "Storage_error"

let q_count_repositories =
  (Caqti_type.int64 ->! Caqti_type.int)
    "SELECT COUNT(*) FROM project_repositories WHERE project_id = $1"
