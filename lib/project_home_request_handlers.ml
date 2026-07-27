(* HTTP layer for the existing-community home request of one verified
   permanent project (GET and POST /projects/:slug/request-home), kept out
   of the legacy Handlers macro-module per the feature-module guideline.
   Both handlers are factories over the closed onboarding mode (and, for
   the POST, a config loader) so tests can inject fixed values without
   touching the process environment, and disabled or rejected requests
   never trigger configuration reads or SQL. Nothing here logs a query or
   form value, and no submitted value — target community id, request
   note — is ever placed in a URL, cookie, or log: failed submissions
   re-render over POST instead of redirecting. *)

module Read = Project_home_choice_read_model
module Form = Project_home_request_form
module Store = Project_home_request_store
module Pages_phc = Project_home_choice_pages

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

(* Every rendered page reflects session identity and private workflow
   state: never cacheable, and never leaking onward as a cross-origin
   Referer. The
   referrer policy is Request_origin's shared value rather than
   "no-referrer": these pages host the forms posting to this feature's
   origin-gated routes, and a no-referrer document makes the browser send
   Origin: null on that POST. Cross-origin Referers stay fully suppressed. *)
let page_headers =
  [ ("Cache-Control", "no-store");
    ("Referrer-Policy", Request_origin.referrer_policy);
  ]

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

(* One generic 500 for every read-model or store failure past the gates —
   no Caqti/PostgreSQL detail, error constructor, or fixture value ever
   reaches the page. *)
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
         "Home requests are temporarily unavailable. Please try again later."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 404 for every unavailable project — nonexistent, foreign,
   non-steward, stale, and revoked stay indistinguishable, and no project
   name or slug state leaks through it. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* The PRG destination is this same permanent route, rebuilt structurally
   from the canonical route slug — never by reflecting other request
   data. *)
let request_home_redirect ~slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:(String.concat "/" [ ""; "projects"; slug; "request-home" ])
          ()))

(* The route parameter, absent when the handler runs outside a router (as
   in the DB-free gate tests) — treated like any other unavailable
   project. *)
let route_slug request =
  match Dream.param request "slug" with
  | value -> Some value
  | exception _ -> None

(* --- Read model → page view models (mechanical, public accessors only;
   relation ids, requester, private note, and timestamps deliberately
   never cross) --- *)

let visibility_of_read = function
  | Read.Public -> Pages_phc.Public
  | Read.Unlisted -> Pages_phc.Unlisted
  | Read.Currently_unavailable -> Pages_phc.Currently_unavailable

let community_of_read community : Pages_phc.community =
  {
    Pages_phc.id = Read.community_id community;
    name = Read.community_name community;
    slug = Read.community_slug community;
    description = Read.community_description community;
    visibility = visibility_of_read (Read.community_visibility community);
  }

let project_of_read project : Pages_phc.project =
  {
    Pages_phc.name = Read.project_name project;
    slug = Read.project_slug project;
    namespace_login = Read.project_namespace_login project;
  }

(* Only Pending and Accepted may cross the active-relation accessor; any
   other value means the read-model contract broke, answered as the
   generic corruption 500 — never reconstructed into a rendered state. *)
let state_of_view ~request_note view =
  let project = project_of_read (Read.project view) in
  match Read.active_relation view with
  | Some relation -> (
      let community =
        community_of_read (Read.active_relation_community relation)
      in
      match Read.active_relation_status relation with
      | Project_home_relation.Pending ->
          Ok
            (Pages_phc.Active_relation
               { project; relation = Pages_phc.Pending_request community })
      | Project_home_relation.Accepted ->
          Ok
            (Pages_phc.Active_relation
               {
                 project;
                 relation =
                   Pages_phc.Accepted_home
                     {
                       community;
                       (* The read model's durable derivation, carried
                          across as the bare boolean it is: the raw target
                          lifecycle never reaches the page. *)
                       removal_allowed =
                         Read.active_relation_removal_allowed relation;
                     };
               })
      | Project_home_relation.Rejected | Project_home_relation.Removed ->
          Error ())
  | None -> (
      match Read.eligible_communities view with
      | [] -> Ok (Pages_phc.No_eligible_communities project)
      | communities ->
          Ok
            (Pages_phc.Choose_existing
               {
                 project;
                 communities = List.map community_of_read communities;
                 request_note;
               }))

(* One authorized read-and-render shared by the GET and every recoverable
   POST failure: the current view is always loaded fresh in its own short
   Dream.sql scope — never reused across the store call — so an active
   concurrent relation suppresses the form, newly ineligible targets drop
   out of the chooser, and visibility labels stay current. The live
   request crosses so a rendered form carries a fresh Dream CSRF field. *)
let respond_current_state request ~user_id ~slug ~request_note ~feedback
    ~status =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Read.load_for_steward db ~user_id ~project_slug:slug)
  in
  match loaded with
  (* Nonexistent, foreign, non-steward, stale, and revoked stay
     indistinguishable, and a malformed slug answers identically. *)
  | Ok None | Error Read.Invalid_project_slug -> not_found_page request
  | Error Read.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Read.Inconsistent_data | Read.Storage_error) ->
      server_error_page request
  | Ok (Some view) -> (
      match state_of_view ~request_note view with
      | Error () -> server_error_page request
      | Ok state ->
          Dream.respond ~status ~headers:page_headers
            (Pages_phc.project_home_choice_page
               ?user:(session_user request)
               ~request ~state ~feedback ()))

(* --- GET /projects/:slug/request-home --- *)

let make_project_home_choice_handler ~mode request =
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
            | Some slug ->
                respond_current_state request ~user_id ~slug ~request_note:""
                  ~feedback:None ~status:`OK))

(* --- POST note preservation ---

   Output-only safety gate for re-rendering a rejected submission's note in
   the textarea: valid UTF-8, no NUL, no ASCII control byte except tab and
   LF, no DEL. An over-length note stays preserved untruncated — the domain
   still rejects it — while anything unsafe is dropped whole, never
   repaired. This is not domain validation and makes nothing acceptable. *)
let preservable_note note =
  if
    String.is_valid_utf_8 note
    && String.for_all
         (fun byte ->
           let code = Char.code byte in
           code = 0x09 || code = 0x0a || (code >= 0x20 && code <> 0x7f))
         note
  then note
  else ""

(* --- Store-result mapping --- *)

let handle_store_result request ~user_id ~slug ~submitted_note = function
  | Ok _created ->
      (* The store committed the pending relation together with its audit
         event and its notifications: the one submission boundary. The
         relation id, the target community id, the project slug and the
         private note deliberately do not cross — only the acting steward,
         who is already the distinct id. Consent-gated and best-effort. *)
      Analytics.capture_if_consented request
        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
        (Analytics.Project_home_request_submitted { user_id });
      (* PRG to the same permanent route: the relation id, target id, and
         submitted note deliberately never enter the URL. *)
      Lwt.return (request_home_redirect ~slug)
  | Error (Store.Invalid_project_slug | Store.Project_unavailable) ->
      (* Missing, foreign, unstewarded, stale, and revoked collapse to the
         one generic 404. *)
      not_found_page request
  | Error
      (Store.Invalid_user_id | Store.Invalid_community_id
      | Store.Invalid_relation) ->
      (* Defensive: all three were validated before the store ran. *)
      server_error_page request
  | Error Store.Community_unavailable ->
      (* The reloaded view no longer lists the submitted target; nothing
         reinserts it or names why it became unavailable. *)
      respond_current_state request ~user_id ~slug
        ~request_note:(preservable_note submitted_note)
        ~feedback:(Some Pages_phc.Community_unavailable) ~status:`Conflict
  | Error Store.Active_home_exists ->
      (* The reloaded state is authoritative: an active relation renders
         with no form, so no note is preserved. *)
      respond_current_state request ~user_id ~slug ~request_note:""
        ~feedback:(Some Pages_phc.Active_home_exists) ~status:`Conflict
  | Error (Store.Inconsistent_data | Store.Storage_error) ->
      (* A server failure is never disguised as a user request error. *)
      server_error_page request

let handle_parsed_form request ~user_id ~slug form =
  match Form.create_relation form with
  | Error Project_home_relation.Invalid_request_note ->
      respond_current_state request ~user_id ~slug
        ~request_note:(preservable_note (Form.request_note form))
        ~feedback:(Some Pages_phc.Request_form_invalid) ~status:(`Status 422)
  | Error Project_home_relation.Invalid_transition ->
      (* Impossible from create_pending; kept defensive. *)
      server_error_page request
  | Ok relation ->
      (* One short Dream.sql scope; the store owns its own transaction,
         authorization, target eligibility, and active-home locking. *)
      let%lwt created =
        Dream.sql request (fun db ->
            Store.create db ~user_id ~project_slug:slug
              ~target_community_id:(Form.target_community_id form)
              ~relation)
      in
      handle_store_result request ~user_id ~slug
        ~submitted_note:(Form.request_note form) created

(* --- POST /projects/:slug/request-home --- *)

let make_project_home_request_handler ~mode ~load_config request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no route read, no configuration read, no form read,
         no SQL. *)
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
                match load_config () with
                | Error _ -> unavailable_page request
                | Ok config ->
                    if not (Request_origin.same_origin_request config request)
                    then forbidden_page request
                    else (
                      (* Dream's form API enforces the URL-encoded content
                         type and verifies its own CSRF field, which it
                         strips from the returned fields — so the strict
                         parser below sees application fields only. Every
                         CSRF failure collapses to one generic 403; no
                         framework diagnostic or submitted value reaches
                         the response. *)
                      match%lwt Dream.form request with
                      | `Wrong_content_type -> bad_request_page request
                      | `Expired _ | `Wrong_session _ | `Invalid_token _
                      | `Missing_token _ | `Many_tokens _ ->
                          forbidden_page request
                      | `Ok fields -> (
                          match Form.of_fields fields with
                          | Error Form.Invalid_form ->
                              (* A structurally invalid submission never
                                 reaches the store; the current state
                                 re-renders with nothing malformed
                                 reflected. *)
                              respond_current_state request ~user_id ~slug
                                ~request_note:""
                                ~feedback:(Some Pages_phc.Request_form_invalid)
                                ~status:`Bad_Request
                          | Ok form ->
                              handle_parsed_form request ~user_id ~slug form)))))
