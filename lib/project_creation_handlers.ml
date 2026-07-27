(* HTTP layer for permanent project creation (POST /projects) and the
   permanent setup destination (GET /projects/:slug/setup), kept out of the
   legacy Handlers macro-module per the feature-module guideline. Both
   handlers are factories over the closed onboarding mode (and, for the
   POST, a config loader) so tests can inject fixed values without touching
   the process environment, and disabled or rejected requests never trigger
   configuration reads or SQL. Nothing here logs a query or form value, and
   no submitted identity value is ever placed in a URL, cookie, or log —
   failed submissions re-render over POST instead of redirecting. *)

module Read = Project_onboarding_draft_read_model
module Form = Project_identity_form
module Fin = Project_finalization_store
module Home_read = Project_home_setup_read_model
module Pages_ps = Project_setup_pages
module Pages_home = Project_home_setup_pages

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

(* Every rendered page in this feature reflects session identity and
   submitted or permanent project state: never cacheable, and never leaking
   onward as a Referer. *)
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

let forbidden_page request =
  Dream.respond ~status:`Forbidden ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Allowed"
       ~message:"This request is not allowed." ~alert_type:"error"
       ~return_url:"/projects/new" request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* One generic 500 for every read-model or store failure past the gates —
   no Caqti/PostgreSQL detail, error constructor, or fixture value ever
   reaches the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* Configuration-loading failure on the POST: a deployment problem, answered
   generically and never disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:
         "Project creation is temporarily unavailable. Please try again \
          later."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* One generic 404 for every unavailable permanent project — nonexistent,
   foreign, non-steward, stale, and revoked stay indistinguishable, and no
   project name or slug state leaks through it. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* --- PRG targets, built structurally — never by concatenating request
   data. Only an owner-authorized draft id or a store-persisted canonical
   slug ever reaches a Location. --- *)

let feedback_redirect selection =
  clean_redirect
    (Uri.to_string
       (Uri.make ~path:"/projects/new"
          ~query:[ ("selection", [ selection ]) ]
          ()))

let draft_redirect ~draft_id selection =
  clean_redirect
    (Uri.to_string
       (Uri.make ~path:"/projects/new"
          ~query:
            [ ("draft", [ Int64.to_string draft_id ]);
              ("selection", [ selection ]);
            ]
          ()))

(* The permanent PRG destination: canonical slug only — no project id,
   draft id, success flag, or submitted value. *)
let setup_redirect ~slug =
  clean_redirect
    (Uri.to_string
       (Uri.make ~path:(String.concat "/" [ ""; "projects"; slug; "setup" ]) ()))

(* --- Read model → identity-step view models (mechanical, public accessors
   only; installation ids, account ids, GitHub repository ids, provenance,
   and timestamps deliberately never cross) --- *)

let account_type_of_read = function
  | Github_user_installations.User -> Pages_ps.Personal
  | Github_user_installations.Organization -> Pages_ps.Organization

let draft_option_of_summary summary : Pages_ps.draft_option =
  {
    Pages_ps.draft_id = Read.draft_id summary;
    account_login = Read.account_login summary;
    account_type = account_type_of_read (Read.account_type summary);
    repository_count = Read.repository_count summary;
    selected_repository_count = Read.selected_repository_count summary;
  }

let identity_repository_of_read repository : Pages_ps.identity_repository =
  {
    Pages_ps.snapshot_id = Read.snapshot_id repository;
    full_name = Read.full_name repository;
    is_archived = Read.is_archived repository;
  }

(* Only currently selected rows cross into the identity step, in snapshot
   order. *)
let identity_configuration_of_view ~values view :
    Pages_ps.identity_configuration =
  {
    Pages_ps.draft = draft_option_of_summary (Read.summary view);
    selected_repositories =
      List.filter_map
        (fun repository ->
          if Read.is_selected repository then
            Some (identity_repository_of_read repository)
          else None)
        (Read.repositories view);
    values;
  }

(* Neutral prefills for the recovered-draft re-render of a structurally
   invalid submission: malformed text is deliberately not preserved. *)
let neutral_identity_values : Pages_ps.identity_values =
  {
    Pages_ps.kind = Project_identity.Project;
    name = "";
    slug = "";
    description = "";
    website_url = "";
    primary_snapshot_id = None;
  }

(* A failed-but-parsed submission re-renders byte-exact through the public
   form accessors — never through the raw body. *)
let identity_values_of_form form : Pages_ps.identity_values =
  {
    Pages_ps.kind = Form.kind form;
    name = Form.name form;
    slug = Form.slug form;
    description = Form.description form;
    website_url = Form.website_url form;
    primary_snapshot_id = Form.primary_snapshot_id form;
  }

(* Direct render over the POST: user-entered identity values never travel
   through a redirect, query parameter, or cookie. The live request is
   passed so the re-rendered form carries a fresh Dream CSRF field. *)
let respond_identity_page request ~status ~feedback ~values view =
  Dream.respond ~status ~headers:page_headers
    (Pages_ps.project_setup_page
       ?user:(session_user request)
       ~request
       ~state:
         (Pages_ps.Configure_identity
            (identity_configuration_of_view ~values view))
       ~feedback:(Some feedback) ())

let has_selection view = List.exists Read.is_selected (Read.repositories view)

let load_available request ~user_id ~draft_id =
  Dream.sql request (fun db -> Read.load_available db ~user_id ~draft_id)

(* --- Strict draft-id recovery for structurally invalid submissions --- *)

(* Strict positive decimal int64: ASCII digits only, must fit int64, must be
   positive. Leading zeroes are tolerated — they cannot alias a different id
   — but signs, whitespace, decimal points, hex, and overflow are
   rejected. *)
let positive_int64_of_digits value =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  if String.equal value "" || not (String.for_all is_digit value) then None
  else
    let rec accumulate i acc =
      if i = String.length value then Some acc
      else
        let d = Int64.of_int (Char.code value.[i] - Char.code '0') in
        if Int64.compare acc (Int64.div (Int64.sub Int64.max_int d) 10L) > 0
        then None
        else accumulate (i + 1) (Int64.add (Int64.mul acc 10L) d)
    in
    match accumulate 0 0L with
    | Some id when Int64.compare id 0L > 0 -> Some id
    | _ -> None

(* Non-authoritative routing recovery only: exactly one draft_id field with
   a strict positive decimal value. The id means nothing until
   load_available re-authorizes it for the current user. *)
let recovered_draft_id fields =
  match
    List.filter (fun (name, _) -> String.equal name "draft_id") fields
  with
  | [ (_, value) ] -> positive_int64_of_digits value
  | _ -> None

(* A structurally invalid submission never reaches the finalization store.
   With a recoverable owner-authorized draft that still has a selection,
   the identity step re-renders with neutral values and the generic invalid
   feedback; an authorized draft with an empty selection takes the
   repository-step redirect; every other shape — unparseable, foreign,
   unavailable — is one generic 400 that discloses nothing about the
   submitted id, which never enters a redirect. *)
let handle_invalid_form request ~user_id fields =
  match recovered_draft_id fields with
  | None -> bad_request_page request
  | Some draft_id -> (
      let%lwt loaded = load_available request ~user_id ~draft_id in
      match loaded with
      | Ok None -> bad_request_page request
      | Error (Read.Invalid_user_id | Read.Invalid_draft_id) ->
          (* Defensive: both were validated before the read. *)
          bad_request_page request
      | Error (Read.Inconsistent_data | Read.Storage_error) ->
          server_error_page request
      | Ok (Some view) ->
          if has_selection view then
            respond_identity_page request ~status:`Bad_Request
              ~feedback:Pages_ps.Identity_form_invalid
              ~values:neutral_identity_values view
          else Lwt.return (draft_redirect ~draft_id "required"))

(* --- Domain-validation and finalization-conflict rendering --- *)

(* Field-level identity errors are user-correctable: re-render the
   submitted values over the current owner-authorized view as a 422. The
   two selected-context errors cannot come from user input here — the
   selection was just derived from the read model — so they map to the
   generic 500 and the repository-step redirect. *)
let handle_identity_error request ~draft_id ~form ~view error =
  let rerender feedback =
    respond_identity_page request ~status:(`Status 422) ~feedback
      ~values:(identity_values_of_form form) view
  in
  match error with
  | Project_identity.Invalid_name -> rerender Pages_ps.Identity_name_invalid
  | Project_identity.Invalid_slug -> rerender Pages_ps.Identity_slug_invalid
  | Project_identity.Reserved_slug -> rerender Pages_ps.Identity_slug_reserved
  | Project_identity.Invalid_description ->
      rerender Pages_ps.Identity_description_invalid
  | Project_identity.Invalid_website_url ->
      rerender Pages_ps.Identity_website_invalid
  | Project_identity.Invalid_primary_repository ->
      rerender Pages_ps.Identity_primary_invalid
  | Project_identity.Primary_repository_required ->
      rerender Pages_ps.Identity_primary_required
  | Project_identity.Invalid_repository_selection -> server_error_page request
  | Project_identity.No_repositories_selected ->
      Lwt.return (draft_redirect ~draft_id "required")

(* A re-renderable finalization conflict always reloads the current draft
   view first — the pre-finalization view may describe a snapshot a
   concurrent refresh or selection update replaced, and stale primary
   options must never be offered. *)
let rerender_after_conflict request ~user_id ~draft_id ~form feedback =
  let%lwt reloaded = load_available request ~user_id ~draft_id in
  match reloaded with
  | Error _ -> server_error_page request
  | Ok None -> Lwt.return (feedback_redirect "unavailable")
  | Ok (Some view) ->
      if has_selection view then
        respond_identity_page request ~status:`Conflict ~feedback
          ~values:(identity_values_of_form form) view
      else Lwt.return (draft_redirect ~draft_id "required")

(* [identity] and [repository_count] are carried in for the success branch
   alone: the finalized project's kind and the size of the authoritative
   selection the store finalized. Neither reaches a page, a URL, or a log. *)
let handle_finalization_result request ~user_id ~draft_id ~form ~identity
    ~repository_count = function
  | Ok created ->
      (* Finalization committed and the permanent project is in hand: the
         anchor boundary of both community-home funnels. The permanent row id
         comes from the store's own public accessor; the canonical slug,
         name, description, website, and every GitHub identifier deliberately
         do not cross. Consent-gated and best-effort — a PostHog failure
         cannot change this redirect. *)
      Analytics.capture_if_consented request
        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
        (Analytics.Github_project_created
           {
             user_id;
             project_id = Fin.project_id created;
             project_kind = Project_identity.kind identity;
             repository_count;
           });
      (* The permanent PRG destination: only the canonical persisted slug
         crosses. *)
      Lwt.return (setup_redirect ~slug:(Fin.project_slug created))
  | Error Fin.Draft_unavailable ->
      (* Possibly completed by a racing replay, foreign, or expired: the
         submitted id is dropped from the URL and nothing about the
         permanent outcome leaks. *)
      Lwt.return (feedback_redirect "unavailable")
  | Error Fin.No_repositories_selected ->
      Lwt.return (draft_redirect ~draft_id "required")
  | Error Fin.Selection_stale ->
      (* Back to the repository step — the identity step would offer the
         same stale primary options. *)
      Lwt.return (draft_redirect ~draft_id "stale")
  | Error Fin.Kind_namespace_mismatch ->
      rerender_after_conflict request ~user_id ~draft_id ~form
        Pages_ps.Identity_namespace_mismatch
  | Error Fin.Slug_unavailable ->
      rerender_after_conflict request ~user_id ~draft_id ~form
        Pages_ps.Identity_slug_unavailable
  | Error Fin.Repository_already_connected ->
      rerender_after_conflict request ~user_id ~draft_id ~form
        Pages_ps.Identity_repository_already_connected
  | Error (Fin.Invalid_user_id | Fin.Invalid_draft_id) ->
      (* Defensive: both were validated before finalize was called. *)
      server_error_page request
  | Error (Fin.Inconsistent_data | Fin.Storage_error) ->
      (* A server failure is never disguised as a user input error. *)
      server_error_page request

(* A structurally valid submission: the owner-authorized current draft view
   is authoritative for the selected snapshot ids — never the browser. The
   read and the finalization run in separate short Dream.sql scopes on
   purpose: the store revalidates everything under its own lock, and no
   pooled connection is retained between the two. *)
let handle_parsed_form request ~user_id form =
  let draft_id = Form.draft_id form in
  let%lwt loaded = load_available request ~user_id ~draft_id in
  match loaded with
  | Ok None -> Lwt.return (feedback_redirect "unavailable")
  | Error
      ( Read.Invalid_user_id | Read.Invalid_draft_id | Read.Inconsistent_data
      | Read.Storage_error ) ->
      server_error_page request
  | Ok (Some view) -> (
      let selected = List.filter Read.is_selected (Read.repositories view) in
      match List.map Read.snapshot_id selected with
      | [] -> Lwt.return (draft_redirect ~draft_id "required")
      | selected_snapshot_ids -> (
          match Form.create_identity form ~selected_snapshot_ids with
          | Error error ->
              handle_identity_error request ~draft_id ~form ~view error
          | Ok identity ->
              let%lwt finalized =
                Dream.sql request (fun db ->
                    Fin.finalize db ~user_id ~draft_id ~identity)
              in
              handle_finalization_result request ~user_id ~draft_id ~form
                ~identity
                ~repository_count:(List.length selected_snapshot_ids)
                finalized))

(* --- POST /projects --- *)

let make_project_creation_handler ~mode ~load_config request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no configuration read, no form read, no SQL. *)
      Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Lwt.return (login_redirect ())
      | Some user_id ->
          let is_admin = session_field_opt request "is_admin" = Some "true" in
          if not (Project_onboarding.onboarding_available mode ~is_admin) then
            Lwt.return (bring_redirect ())
          else (
            match load_config () with
            | Error _ -> unavailable_page request
            | Ok config ->
                if not (Request_origin.same_origin_request config request)
                then forbidden_page request
                else (
                  (* Dream's form API enforces the URL-encoded content type
                     and verifies its own CSRF field, which it strips from
                     the returned fields — so the strict parser below sees
                     application fields only. Every CSRF failure collapses
                     to one generic 403; no framework diagnostic or
                     submitted value reaches the response. *)
                  match%lwt Dream.form request with
                  | `Wrong_content_type -> bad_request_page request
                  | `Expired _ | `Wrong_session _ | `Invalid_token _
                  | `Missing_token _ | `Many_tokens _ ->
                      forbidden_page request
                  | `Ok fields -> (
                      match Form.of_fields fields with
                      | Error Form.Invalid_form ->
                          handle_invalid_form request ~user_id fields
                      | Ok form -> handle_parsed_form request ~user_id form))))

(* --- GET /projects/:slug/setup --- *)

let account_type_of_home = function
  | Github_user_installations.User -> Pages_home.Personal
  | Github_user_installations.Organization -> Pages_home.Organization

(* Mechanical read model → page mapping; the local project id deliberately
   never crosses into the view model. *)
let home_repository_of_read repository : Pages_home.repository =
  {
    Pages_home.full_name = Home_read.full_name repository;
    html_url = Home_read.html_url repository;
    description = Home_read.repository_description repository;
    default_branch = Home_read.default_branch repository;
    is_primary = Home_read.is_primary repository;
    is_archived = Home_read.is_archived repository;
  }

let home_project_of_read project : Pages_home.project =
  {
    Pages_home.name = Home_read.name project;
    slug = Home_read.slug project;
    description = Home_read.description project;
    website_url = Home_read.website_url project;
    kind = Home_read.kind project;
    namespace_login = Home_read.namespace_login project;
    namespace_type = account_type_of_home (Home_read.namespace_type project);
    repositories =
      List.map home_repository_of_read (Home_read.repositories project);
  }

(* The route parameter, absent when the handler runs outside a router (as
   in the DB-free gate tests) — treated like any other invalid slug. *)
let route_slug request =
  match Dream.param request "slug" with
  | value -> Some value
  | exception _ -> None

let make_project_home_setup_handler ~mode request =
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
                      Home_read.load_for_steward db ~user_id ~slug)
                in
                match loaded with
                (* Nonexistent, foreign, non-steward, stale, and revoked
                   stay indistinguishable, and a malformed slug answers
                   identically. *)
                | Ok None | Error Home_read.Invalid_slug ->
                    not_found_page request
                | Error
                    ( Home_read.Invalid_user_id | Home_read.Inconsistent_data
                    | Home_read.Storage_error ) ->
                    server_error_page request
                | Ok (Some project) ->
                    Dream.respond ~status:`OK ~headers:page_headers
                      (Pages_home.project_home_setup_page
                         ?user:(session_user request)
                         ~request
                         ~project:(home_project_of_read project)
                         ()))))
