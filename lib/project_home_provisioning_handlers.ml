(* HTTP layer for the dedicated-community-home creation flow of one verified
   permanent project (GET /projects/:slug/community-home/new and POST
   /projects/:slug/community-home), kept out of the legacy Handlers
   macro-module per the feature-module guideline. Both handlers are factories
   over the closed onboarding mode (and, for the POST, a config loader) so
   tests can inject fixed values without touching the process environment, and
   a disabled or rejected request never triggers a route read, a configuration
   read, or SQL. Nothing here logs a query or form value, and no submitted
   value — community name, slug, or description — is ever placed in a URL,
   cookie, or log: failed submissions re-render over POST instead of
   redirecting. *)

module Read = Project_home_provisioning_read_model
module Form = Project_home_provisioning_form
module Store = Project_home_provisioning_store
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
   negative reads as "not logged in". A session is_admin claim has no meaning
   without a valid positive session user id. *)
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

let forbidden_page request =
  Dream.respond ~status:`Forbidden ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Allowed"
       ~message:"This request is not allowed." ~alert_type:"error"
       ~return_url:"/" request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 404 for every unavailable project — nonexistent, foreign,
   non-steward, stale, revoked, and already carrying an active home relation
   stay indistinguishable, and no project name or slug state leaks through
   it. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* One generic 500 for every read-model or store failure past the gates — no
   Caqti/PostgreSQL detail, error constructor, or fixture value ever reaches
   the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* Configuration-loading failure on the POST: a deployment problem, answered
   generically and never disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:
         "Creating a community home is temporarily unavailable. Please try \
          again later."
       ~alert_type:"error" ~return_url:"/" request)

(* --- Redirect destinations ---

   Both are built structurally from a canonical slug alone — the created
   community's, straight from the store, or this route's own project slug,
   which the store already proved canonical — never by reflecting other
   request data, and neither carries a result, feedback, or return-URL
   parameter. *)

(* The existing canonical private-community management route. The creating
   steward is already a member and the community's top moderator, so the
   existing private-community authorization admits them; the new community is
   a private setup draft that still needs configuration and publication,
   which is why the public feed and /c/:slug are deliberately not the
   post-provisioning destination. *)
let community_settings_redirect ~community_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:(String.concat "/" [ ""; "c"; community_slug; "settings" ])
          ()))

(* The authoritative current-home GET: it already renders a pending request
   or a connected home, which is exactly what a project with an active
   relation must see instead of a stale creation form. *)
let request_home_redirect ~project_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:
            (String.concat "/" [ ""; "projects"; project_slug; "request-home" ])
          ()))

(* The route parameter, absent when the handler runs outside a router (as in
   the DB-free gate tests) — treated like any other unavailable project.
   Never trimmed, lowercased, repaired, or percent-decoded by hand: the form
   and the store own validation. *)
let route_slug request =
  match Dream.param request "slug" with
  | value -> Some value
  | exception _ -> None

(* --- Shared rollout and authentication gates ---

   Identical order and semantics to the other GitHub-project handlers: mode
   Off first (kill switch, no route read, no SQL), then a valid positive
   session user id, then Admins/Public rollout eligibility — all before any
   route parameter, configuration load, origin check, form read, or SQL
   scope. *)
let with_gates ~mode request k =
  match mode with
  | Project_onboarding.Off -> Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Lwt.return (login_redirect ())
      | Some user_id ->
          let is_admin = session_field_opt request "is_admin" = Some "true" in
          if not (Project_onboarding.onboarding_available mode ~is_admin) then
            Lwt.return (bring_redirect ())
          else k ~user_id)

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

(* --- Safe form-value preservation ---

   The rejected submission's own three values, returned only when the field
   set is provably exactly one occurrence of each expected name and nothing
   else — the same structural shape the parser accepts. Any other shape (an
   unknown key, a duplicate, a missing field) yields blanks, so a structural
   rejection reflects nothing a client planted. Values are never trimmed,
   canonicalized, or repaired here; the page escapes them at the template
   boundary. Dream has already stripped its own CSRF field, so it can never
   appear among these. *)
let blank_values : Pages_phv.form_values =
  { Pages_phv.community_name = ""; community_slug = ""; community_description = "" }

let submitted_values fields : Pages_phv.form_values =
  let exactly_once name =
    match List.filter (fun (key, _) -> String.equal key name) fields with
    | [ (_, value) ] -> Some value
    | _ -> None
  in
  if List.length fields <> 3 then blank_values
  else
    match
      ( exactly_once "community_name",
        exactly_once "community_slug",
        exactly_once "community_description" )
    with
    | Some name, Some slug, Some description ->
        { Pages_phv.community_name = name;
          community_slug = slug;
          community_description = description;
        }
    | _ -> blank_values

(* --- Owner-authorized read-and-render ---

   One helper shared by the GET and every recoverable POST failure: the
   project is always re-authorized fresh in its own short Dream.sql scope —
   never reused across the store call — so stewardship loss, verification
   drift, and a concurrently created active relation all take effect before
   anything private renders again. The live request crosses so the rendered
   form carries a fresh Dream CSRF field, and [values_of] lets the GET
   prefill from the read model's suggestions while the POST reflects the
   submission it just rejected. *)
let respond_owner_authorized request ~user_id ~project_slug ~values_of ~feedback
    ~status =
  let%lwt loaded =
    Dream.sql request (fun db -> Read.load_for_steward db ~user_id ~project_slug)
  in
  match loaded with
  (* Nonexistent, foreign, non-steward, stale, revoked, pending, and
     already-homed stay indistinguishable, and a malformed slug answers
     identically. *)
  | Ok None | Error Read.Invalid_project_slug -> not_found_page request
  | Error Read.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Read.Inconsistent_data | Read.Storage_error) ->
      server_error_page request
  | Ok (Some view) ->
      Dream.respond ~status ~headers:page_headers
        (Pages_phv.project_home_provisioning_page ?user:(session_user request)
           ~request
           ~project:(project_of_read (Read.project view))
           ~values:(values_of view) ~feedback ())

(* --- GET /projects/:slug/community-home/new --- *)

let make_project_home_provisioning_page_handler ~mode request =
  with_gates ~mode request (fun ~user_id ->
      match route_slug request with
      | None -> not_found_page request
      | Some slug ->
          respond_owner_authorized request ~user_id ~project_slug:slug
            ~values_of:values_of_view ~feedback:None ~status:`OK)

(* --- Store-result mapping --- *)

let handle_store_result request ~user_id ~slug ~values = function
  | Ok home ->
      if Store.resulting_status home <> Project_home_relation.Accepted then
        (* A committed result that is not Accepted is a broken store
           invariant, not a public state: the generic 500, never a redirect
           that names it. *)
        server_error_page request
      else
        (* The provisioning transaction committed: the private setup draft,
           its initial role/shell, the accepted home relation and the audit
           event all exist. The created community's slug, name and
           description deliberately do not cross — and neither does a
           community id, which the store's narrow result does not carry.
           Consent-gated and best-effort. *)
        (Analytics.capture_if_consented request
           ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
           (Analytics.Dedicated_home_provisioned { user_id });
         (* PRG into the new private setup draft. The canonical slug comes
            only from the store; no submitted value, id, or result token
            enters the URL. *)
         Lwt.return
           (community_settings_redirect
              ~community_slug:(Store.community_slug home)))
  | Error Store.Project_unavailable ->
      (* Missing, foreign, unstewarded, stale, revoked, and a durable admin
         who is not a steward collapse to the one generic 404 — never a
         re-rendered, possibly unauthorized form. *)
      not_found_page request
  | Error Store.Invalid_project_slug ->
      (* Defensive: a malformed route slug is the same generic 404, never
         reflected. *)
      not_found_page request
  | Error Store.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error Store.Community_slug_unavailable ->
      (* Recoverable: the steward edits the slug and retries. Nothing names
         the conflicting community's id, visibility, lifecycle, owner, or
         network status. *)
      respond_owner_authorized request ~user_id ~project_slug:slug
        ~values_of:(fun _ -> values)
        ~feedback:(Some Pages_phv.Community_slug_unavailable)
        ~status:`Conflict
  | Error Store.Active_home_exists ->
      (* A pre-existing or concurrently committed relation — including a
         replayed successful submission. The authoritative current-home GET
         renders pending or accepted state; re-rendering the creation form
         from stale pre-POST state would be a lie. *)
      Lwt.return (request_home_redirect ~project_slug:slug)
  | Error (Store.Inconsistent_data | Store.Storage_error) ->
      (* A server failure is never disguised as a recoverable user error. *)
      server_error_page request

(* --- POST /projects/:slug/community-home --- *)

let make_project_home_provisioning_handler ~mode ~load_config request =
  with_gates ~mode request (fun ~user_id ->
      match route_slug request with
      | None -> not_found_page request
      | Some slug -> (
          match load_config () with
          | Error _ -> unavailable_page request
          | Ok config ->
              if not (Request_origin.same_origin_request config request) then
                forbidden_page request
              else (
                (* Dream's form API enforces the URL-encoded content type and
                   verifies its own CSRF field, which it strips from the
                   returned fields — so the strict parser below sees
                   application fields only. Every CSRF failure collapses to
                   one generic 403; no framework diagnostic or submitted
                   value reaches the response. *)
                match%lwt Dream.form request with
                | `Wrong_content_type -> bad_request_page request
                | `Expired _ | `Wrong_session _ | `Invalid_token _
                | `Missing_token _ | `Many_tokens _ ->
                    forbidden_page request
                | `Ok fields -> (
                    (* No SQL opens before the identity is structurally and
                       semantically valid; the parser owns the whole
                       grammar, and nothing is trimmed or repaired here. *)
                    let reject feedback ~values =
                      respond_owner_authorized request ~user_id
                        ~project_slug:slug
                        ~values_of:(fun _ -> values)
                        ~feedback:(Some feedback) ~status:(`Status 422)
                    in
                    match Form.of_fields fields with
                    | Error Form.Invalid_form ->
                        (* Structurally invalid: the field set cannot be
                           proven safe, so nothing at all is reflected. *)
                        reject Pages_phv.Invalid_form ~values:blank_values
                    | Error Form.Invalid_community_name ->
                        reject Pages_phv.Invalid_community_name
                          ~values:(submitted_values fields)
                    | Error Form.Invalid_community_slug ->
                        reject Pages_phv.Invalid_community_slug
                          ~values:(submitted_values fields)
                    | Error Form.Invalid_community_description ->
                        reject Pages_phv.Invalid_community_description
                          ~values:(submitted_values fields)
                    | Ok identity ->
                        (* One short Dream.sql scope and no outer
                           transaction: the store owns its own transaction,
                           locking, authorization, slug and active-home
                           arbitration, creation, commit, and rollback.
                           Nothing is pre-checked here. *)
                        let%lwt result =
                          Dream.sql request (fun db ->
                              Store.provision db ~actor_user_id:user_id
                                ~project_slug:slug ~identity)
                        in
                        handle_store_result request ~user_id ~slug
                          ~values:(submitted_values fields) result))))
