(* HTTP layer for the moderator review of pending project-home requests
   (GET /c/:slug/project-home-requests and the accept/reject POSTs), kept
   out of the legacy Handlers macro-module per the feature-module
   guideline. All three handlers are factories over the closed onboarding
   mode (and, for the POSTs, a config loader) so tests can inject fixed
   values without touching the process environment, and disabled or
   unauthenticated requests never trigger configuration reads or SQL.

   Authorization is never decided here: the read model and the
   transactional review store each reauthorize entirely in SQL (a current
   top_mod of the target community, or a durable users.is_admin). The
   handler collapses every unauthorized or missing outcome to one generic
   404 so a community slug cannot become an authorization oracle. Nothing
   here logs a query, a note, or an id, and no route or form value is ever
   reflected into a URL, cookie, or log — every recoverable failure reloads
   the authoritative queue over its own short SQL scope rather than reusing
   a pre-POST view. *)

module Read = Project_home_review_read_model
module Store = Project_home_review_store
module Pages_phrv = Project_home_review_pages

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
   negative reads as "not logged in". A session is_admin claim has no
   meaning without a valid positive session user id. *)
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
   Referer. The page remains noindex from the page module itself. The
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

(* Configuration-loading failure on the POSTs: a deployment problem,
   answered generically and never disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:
         "Reviewing is temporarily unavailable. Please try again later."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 404 for every unavailable queue — missing community,
   ordinary member, non-member, lower or removed moderator role, moderator
   of another community, and a session-only admin claim all stay
   indistinguishable, and no community name or slug state leaks through
   it. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* The PRG destination is the queue route, rebuilt structurally from the
   canonical community slug — never by reflecting other request data, and
   with no project slug, decision, result status, or feedback token. *)
let queue_redirect ~community_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:
            (String.concat "/"
               [ ""; "c"; community_slug; "project-home-requests" ])
          ()))

(* Route parameters, absent when the handler runs outside a router (as in
   the DB-free gate tests) — treated defensively like any other unavailable
   value. *)
let route_param request name =
  match Dream.param request name with
  | value -> Some value
  | exception _ -> None

(* --- Read model → page view models (mechanical, public accessors only;
   relation, project, community, requester, and reviewer ids deliberately
   never cross) --- *)

let eligibility_of_read = function
  | Read.Eligible -> Pages_phrv.Eligible
  | Read.Currently_ineligible -> Pages_phrv.Currently_ineligible

let verification_of_read = function
  | Read.Verified -> Pages_phrv.Verified
  | Read.Stale -> Pages_phrv.Stale
  | Read.Revoked -> Pages_phrv.Revoked

let repository_of_read repository : Pages_phrv.repository =
  {
    Pages_phrv.full_name = Read.repository_full_name repository;
    html_url = Read.repository_html_url repository;
    is_primary = Read.repository_is_primary repository;
    is_archived = Read.repository_is_archived repository;
  }

let community_of_read community : Pages_phrv.community =
  {
    Pages_phrv.name = Read.community_name community;
    slug = Read.community_slug community;
    host_eligibility =
      eligibility_of_read (Read.community_host_eligibility community);
  }

(* The private request note crosses only from the authorized read model to
   the private moderator page; it is never logged and never reflected. *)
let request_of_read request : Pages_phrv.pending_request =
  {
    Pages_phrv.project_name = Read.project_name request;
    project_slug = Read.project_slug request;
    project_kind = Read.project_kind request;
    namespace_login = Read.project_namespace_login request;
    verification = verification_of_read (Read.project_verification request);
    repositories =
      List.map repository_of_read (Read.project_repositories request);
    requester_name = Read.requester_name request;
    request_note = Read.request_note request;
  }

let state_of_view view : Pages_phrv.state =
  {
    Pages_phrv.community = community_of_read (Read.community view);
    requests = List.map request_of_read (Read.pending_requests view);
  }

(* One authorized read-and-render shared by the GET and every recoverable
   POST failure: the current queue is always loaded fresh in its own short
   Dream.sql scope — never reused across the store call — so a concurrent
   review winner disappears, changed verification is current, and changed
   community eligibility is current. The live request crosses so a rendered
   form carries a fresh Dream CSRF field. Reviewer authorization is inside
   the read-model SQL; a lost authorization during reload collapses to the
   generic 404 exactly like every other non-viewable state. *)
let respond_current_queue request ~user_id ~community_slug ~feedback ~status =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Read.load_for_reviewer db ~reviewer_user_id:user_id ~community_slug)
  in
  match loaded with
  (* Missing community, ordinary member, non-member, mod/legacy_mod,
     moderator of another community, removed/downgraded top moderator, and
     a session-only admin claim all collapse here, and a malformed slug
     answers identically. *)
  | Ok None | Error Read.Invalid_community_slug -> not_found_page request
  | Error Read.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Read.Inconsistent_data | Read.Storage_error) ->
      server_error_page request
  | Ok (Some view) ->
      Dream.respond ~status ~headers:page_headers
        (Pages_phrv.project_home_review_page ?user:(session_user request)
           ~request ~state:(state_of_view view) ~feedback ())

(* --- Shared rollout and authentication gates ---

   Identical order and semantics to the other GitHub-project handlers:
   mode Off first (kill switch, no route read, no SQL), then a valid
   positive session user id, then Admins/Public rollout eligibility — all
   before any route parameter, configuration load, origin check, form
   read, or SQL scope. *)
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

(* --- GET /c/:slug/project-home-requests --- *)

let make_project_home_review_queue_handler ~mode request =
  with_gates ~mode request (fun ~user_id ->
      match route_param request "slug" with
      | None -> not_found_page request
      | Some community_slug ->
          (* Informational: no locks, no state change, no query parameters,
             no flash state. *)
          respond_current_queue request ~user_id ~community_slug
            ~feedback:None ~status:`OK)

(* --- Store-result mapping ---

   The decision comes exclusively from the route-specific handler, never
   from a browser-supplied field; only Project_unavailable and
   Target_ineligible differ between the two routes. The resulting status is
   checked defensively against the route decision but never used to build a
   URL or expose state. *)
let handle_review_result request ~decision ~user_id ~community_slug = function
  | Ok reviewed ->
      let expected =
        match decision with
        | Store.Accept -> Project_home_relation.Accepted
        | Store.Reject -> Project_home_relation.Rejected
      in
      if Store.resulting_status reviewed <> expected then
        (* A decision/result mismatch is a broken invariant, not a public
           state: the generic 500, never a redirect that names it. And no
           event: nothing coherent committed to report. *)
        server_error_page request
      else (
        (* The review committed with the status its route decided, together
           with its audit event and notifications: the one review boundary,
           for accept and reject alike. The actor is the REVIEWER — a
           different person from the requesting steward, which is why this
           step cannot join a person funnel. The project slug, community
           slug, requester, private note and reviewer authority source
           deliberately do not cross. *)
        Analytics.capture_if_consented request
          ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
          (Analytics.Project_home_request_reviewed
             {
               user_id;
               decision =
                 (match decision with
                 | Store.Accept -> Analytics.Review_accepted
                 | Store.Reject -> Analytics.Review_rejected);
             });
        Lwt.return (queue_redirect ~community_slug))
  | Error Store.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Store.Invalid_project_slug | Store.Invalid_community_slug) ->
      (* Malformed route slugs collapse to the generic 404, never
         reflected. *)
      not_found_page request
  | Error (Store.Community_unavailable | Store.Reviewer_unauthorized) ->
      (* Community deletion, loss of top-mod role, durable admin
         revocation, wrong community, and ordinary user stay
         indistinguishable, and the private queue is not reloaded after
         authorization is lost. *)
      not_found_page request
  | Error Store.Review_unavailable ->
      (* The authoritative reloaded queue may no longer contain that
         project (a concurrent review committed first); nothing reinserts
         it from the route. *)
      respond_current_queue request ~user_id ~community_slug
        ~feedback:(Some Pages_phrv.Review_unavailable) ~status:`Conflict
  | Error Store.Project_unavailable -> (
      match decision with
      | Store.Accept ->
          (* Verified at GET, stale or revoked by POST while still pending:
             show the current queue with generic project-unavailable
             copy. *)
          respond_current_queue request ~user_id ~community_slug
            ~feedback:(Some Pages_phrv.Project_unavailable) ~status:`Conflict
      | Store.Reject ->
          (* Rejection is allowed for verified, stale, and revoked
             projects, so this means the project is genuinely absent —
             reload with the generic review-unavailable copy, never
             acceptance-specific wording for a failed rejection. *)
          respond_current_queue request ~user_id ~community_slug
            ~feedback:(Some Pages_phrv.Review_unavailable) ~status:`Conflict)
  | Error Store.Target_ineligible -> (
      match decision with
      | Store.Accept ->
          (* Eligible at GET, ineligible by POST: acceptance stays
             suppressed, rejection stays available, and the specific
             private/draft/legacy reason is never exposed. *)
          respond_current_queue request ~user_id ~community_slug
            ~feedback:(Some Pages_phrv.Target_ineligible) ~status:`Conflict
      | Store.Reject ->
          (* Impossible: rejection does not require host eligibility. *)
          server_error_page request)
  | Error (Store.Inconsistent_data | Store.Storage_error) ->
      (* A server failure is never disguised as a recoverable review
         error. *)
      server_error_page request

(* --- POST /c/:slug/projects/:project_slug/{accept,reject}} ---

   One private decision-specific implementation shared by both mutation
   handlers. After the shared gates: read route parameters, load the config
   (solely to reuse the same public-origin policy), check same-origin,
   parse the framework-CSRF-verified form, require zero application fields,
   then call the transactional review store in one short SQL scope. The
   store owns durable validation, current top-mod/admin authorization, lock
   ordering, verification/eligibility rules, concurrent-review arbitration,
   and commit/rollback — the handler never pre-authorizes through the read
   model. *)
let make_review_post_handler ~decision ~mode ~load_config request =
  with_gates ~mode request (fun ~user_id ->
      match (route_param request "slug", route_param request "project_slug") with
      | None, _ | _, None -> not_found_page request
      | Some community_slug, Some project_slug -> (
          match load_config () with
          | Error _ -> unavailable_page request
          | Ok config ->
              if not (Request_origin.same_origin_request config request) then
                forbidden_page request
              else (
                (* Dream's form API enforces the URL-encoded content type
                   and verifies its own CSRF field, which it strips from
                   the returned fields.

                   Every CSRF failure still refuses the submission with 403
                   and never opens the store, but it is answered with the
                   authorized queue rather than a terminal message page:
                   the framework token lives one hour while the session that
                   renders it lives two weeks, so a queue left open (or
                   served before a restart, which rotates the encryption
                   secret) would otherwise become permanently unactionable.
                   The reload is already past the session, rollout and
                   same-origin gates, re-authorizes the reviewer in the
                   read-model SQL, and carries a fresh token; no framework
                   diagnostic reaches the response. *)
                match%lwt Dream.form request with
                | `Wrong_content_type -> bad_request_page request
                | `Expired _ | `Wrong_session _ | `Invalid_token _
                | `Missing_token _ | `Many_tokens _ ->
                    respond_current_queue request ~user_id ~community_slug
                      ~feedback:(Some Pages_phrv.Stale_form) ~status:`Forbidden
                | `Ok fields -> (
                    (* The review form carries no application field: any
                       remaining value (a browser-supplied decision, an id,
                       a return URL, a duplicate, or an unknown key) is a
                       generic 400 that never reaches the store. *)
                    match fields with
                    | _ :: _ -> bad_request_page request
                    | [] ->
                        let%lwt result =
                          Dream.sql request (fun db ->
                              Store.review db ~reviewer_user_id:user_id
                                ~project_slug
                                ~target_community_slug:community_slug ~decision)
                        in
                        handle_review_result request ~decision ~user_id
                          ~community_slug result))))

let make_project_home_accept_handler ~mode ~load_config request =
  make_review_post_handler ~decision:Store.Accept ~mode ~load_config request

let make_project_home_reject_handler ~mode ~load_config request =
  make_review_post_handler ~decision:Store.Reject ~mode ~load_config request
