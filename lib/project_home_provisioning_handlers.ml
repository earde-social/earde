(* HTTP layer for the dedicated-community-home creation entry of one verified
   permanent project (GET /projects/:slug/community-home/new), kept out of
   the legacy Handlers macro-module per the feature-module guideline. The
   handler is a factory over the closed onboarding mode so tests can inject a
   fixed value without touching the process environment, and a disabled or
   rejected request never triggers a route read or SQL. Nothing here logs a
   query value, and no request value is ever reflected into a redirect. *)

module Read = Project_home_provisioning_read_model
module Pages_phv = Project_home_provisioning_pages

(* --- Session reads (identical rules to the other project-setup handlers) --- *)

(* Session read that also behaves in tests where no session middleware is
   installed: no middleware simply means no authenticated session. *)
let session_field_opt request name =
  match Dream.session_field request name with
  | exception _ -> None
  | value -> value

let session_user request = session_field_opt request "username"

(* The authenticated user, straight from the authoritative Dream session —
   never from caller-supplied input. Anything absent, non-numeric, zero, or
   negative reads as "not logged in". *)
let authenticated_user_id request =
  match session_field_opt request "user_id" with
  | None -> None
  | Some raw -> (
      match int_of_string_opt raw with
      | Some id when id > 0 -> Some id
      | _ -> None)

(* --- Responses --- *)

(* The rendered page reflects session identity and private setup state:
   never cacheable, and never leaking onward as a Referer. *)
let page_headers =
  [ ("Cache-Control", "no-store"); ("Referrer-Policy", "no-referrer") ]

(* Explicit 303 with an empty body and a server-controlled Location — no
   request value is ever reflected into a redirect. *)
let clean_redirect location =
  Dream.response ~status:`See_Other
    ~headers:
      [ ("Location", location);
        ("Cache-Control", "no-store");
        ("Pragma", "no-cache");
        ("Referrer-Policy", "no-referrer");
      ]
    ""

let bring_redirect () = clean_redirect "/bring"

(* Deliberately no return URL: nothing caller-controlled rides along. *)
let login_redirect () = clean_redirect "/login"

(* One generic 404 for every unavailable project — nonexistent, foreign,
   non-steward, stale, revoked, and already carrying an active home relation
   stay indistinguishable, and no project name or slug state leaks through
   it. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* One generic 500 for every read-model failure past the gates — no
   Caqti/PostgreSQL detail, error constructor, or fixture value ever reaches
   the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* The route parameter, absent when the handler runs outside a router (as in
   the DB-free gate tests) — treated like any other unavailable project. *)
let route_slug request =
  match Dream.param request "slug" with
  | value -> Some value
  | exception _ -> None

(* --- Read model → page view models (mechanical, public accessors only) --- *)

let project_of_read project : Pages_phv.project =
  {
    Pages_phv.name = Read.project_name project;
    slug = Read.project_slug project;
    description = Read.project_description project;
    kind = Read.project_kind project;
    namespace_login = Read.project_namespace_login project;
  }

(* First view: the controls carry the read model's suggestions verbatim. An
   absent description suggestion is an empty textarea, not the string
   "None". *)
let values_of_view view : Pages_phv.form_values =
  {
    Pages_phv.community_name = Read.suggested_community_name view;
    community_slug = Read.suggested_community_slug view;
    community_description =
      (match Read.suggested_community_description view with
      | Some text -> text
      | None -> "");
  }

(* --- GET /projects/:slug/community-home/new --- *)

let make_project_home_provisioning_page_handler ~mode request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no route-parameter read, no SQL. *)
      Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Lwt.return (login_redirect ())
      | Some user_id ->
          let is_admin = session_field_opt request "is_admin" = Some "true" in
          if not (Project_onboarding.onboarding_available mode ~is_admin) then
            Lwt.return (bring_redirect ())
          else (
            match route_slug request with
            | None -> not_found_page request
            | Some slug -> (
                let%lwt loaded =
                  Dream.sql request (fun db ->
                      Read.load_for_steward db ~user_id ~project_slug:slug)
                in
                match loaded with
                (* Nonexistent, foreign, non-steward, stale, revoked, and
                   already-homed stay indistinguishable, and a malformed
                   slug answers identically. *)
                | Ok None | Error Read.Invalid_project_slug ->
                    not_found_page request
                | Error Read.Invalid_user_id ->
                    (* Defensive: the session gate already validated the
                       id. *)
                    server_error_page request
                | Error (Read.Inconsistent_data | Read.Storage_error) ->
                    server_error_page request
                | Ok (Some view) ->
                    (* The live request always crosses, so the form the
                       future POST will consume already carries a fresh
                       Dream CSRF field. *)
                    Dream.respond ~status:`OK ~headers:page_headers
                      (Pages_phv.project_home_provisioning_page
                         ?user:(session_user request)
                         ~request
                         ~project:(project_of_read (Read.project view))
                         ~values:(values_of_view view) ~feedback:None ()))))
