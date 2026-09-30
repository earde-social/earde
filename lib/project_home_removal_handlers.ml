(* HTTP layer for removing an accepted project-home relation from either
   authorized surface (the steward's project page and the community
   settings section). Both handlers are factories over the closed
   onboarding mode and a config loader so tests can inject fixed values
   without touching the process environment, and disabled or rejected
   requests never trigger configuration reads or SQL.

   Authorization is never decided here. The transactional removal store
   reauthorizes entirely in SQL against the three durable sources (a
   current project steward row, a current top_mod row on the target
   community, or the durable users.is_admin flag), so the two routes differ
   only in the surface that emitted the form and where the browser returns.
   A top moderator calling the project-shaped route and a steward calling
   the community-shaped one both succeed; the route shape adds no second
   policy.

   Nothing here logs a route value, a form value, or an identifier, and no
   caller-controlled value is ever reflected into a URL, cookie, header, or
   body. *)

module Store = Project_home_removal_store

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
   meaning without a valid positive session user id, and no durable meaning
   at all: the store consults the users.is_admin column. *)
let authenticated_user_id request =
  match session_field_opt request "user_id" with
  | None -> None
  | Some raw -> (
      match int_of_string_opt raw with
      | Some id when id > 0 -> Some id
      | _ -> None)

(* --- Responses --- *)

(* Every rendered page reflects session identity and private workflow
   state: never cacheable, and never leaking onward as a Referer. *)
let page_headers =
  [ ("Cache-Control", "no-store"); ("Referrer-Policy", "no-referrer") ]

(* Explicit 303 with an empty body and a server-controlled Location — no
   request value is ever reflected into a redirect. *)
let clean_redirect location =
  Dream.response ~status:`See_Other
    ~headers:
      [
        ("Location", location);
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
    (Site_pages.msg_page ?user:(session_user request) ~title:"Not Allowed"
       ~message:"This request is not allowed." ~alert_type:"error"
       ~return_url:"/" request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 500 for every store failure past the gates — no
   Caqti/PostgreSQL detail, error constructor, or durable value ever
   reaches the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* Configuration-loading failure: a deployment problem, answered
   generically and never disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:"Removal is temporarily unavailable. Please try again later."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 404 for every unavailable or unauthorized removal — missing
   project, missing community, wrong pair, removed stewardship, unrelated
   steward, mod/legacy_mod, moderator of another community, removed or
   downgraded top moderator, revoked durable admin, and a session-only
   admin claim all stay indistinguishable. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error" ~return_url:"/"
       request)

(* --- Redirect destinations ---

   Both are built structurally from the canonical route slug alone — never
   by reflecting other request data — and neither carries a result,
   feedback, or return-URL parameter. Each destination authoritatively
   re-renders the current durable state, which is what makes a stale or
   replayed removal safe. *)

let request_home_redirect ~project_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:
            (String.concat "/" [ ""; "projects"; project_slug; "request-home" ])
          ()))

(* The existing canonical community settings route with its existing panel
   grammar — the same GET the settings navigation links emit, not a new or
   duplicate settings destination. *)
let community_settings_redirect ~community_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:(String.concat "/" [ ""; "c"; community_slug; "settings" ])
          ~query:[ ("panel", [ "projects" ]) ]
          ()))

(* Route parameters, absent when the handler runs outside a router (as in
   the DB-free gate tests) — treated defensively like any other unavailable
   value. Never trimmed, lowercased, repaired, or percent-decoded by hand:
   the store owns validation. *)
let route_param request name =
  match Dream.param request name with
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

(* --- Store-result mapping ---

   The destination is the surface's own current-state route, used for both
   a committed removal and Removal_unavailable. Collapsing those two
   deliberately removes the error oracle a stale or replayed form would
   otherwise be, performs no second mutation, and lets the destination
   itself show what is durably true. *)
let handle_removal_result request ~user_id ~surface ~redirect = function
  | Ok removed ->
      if Store.resulting_status removed <> Project_home_relation.Removed then
        (* A result that is not Removed is a broken store invariant, not a
           public state: the generic 500, never a redirect that names it.
           And no event: nothing coherent committed to report. *)
        server_error_page request
      else (
        (* The accepted → removed transition committed, together with its
           audit event and notifications: the one removal boundary. Both
           routes reach it through this single private helper, so a removal
           can never be counted twice. Only the route surface crosses —
           never which of steward / top moderator / durable admin authorized
           it, and never a project or community identity. *)
        Analytics.capture_if_consented request
          ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
          (Analytics.Project_home_removed { user_id; removal_surface = surface });
        Lwt.return (redirect ()))
  | Error Store.Removal_unavailable ->
      (* No relation, a pending request, a closed history row, an accepted
         home targeting another community, and a concurrent removal that
         committed first all collapse here — same destination as success,
         but deliberately NO event: nothing was removed. That asymmetry is
         invisible to the client, which sees one redirect either way. *)
      Lwt.return (redirect ())
  | Error Store.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Store.Invalid_project_slug | Store.Invalid_community_slug) ->
      (* Malformed route slugs collapse to the generic 404, never
         reflected. *)
      not_found_page request
  | Error
      ( Store.Project_unavailable | Store.Community_unavailable
      | Store.Actor_unauthorized ) ->
      (* Missing project, missing community, wrong pair, and every
         insufficient authority stay indistinguishable. *)
      not_found_page request
  | Error (Store.Inconsistent_data | Store.Storage_error) ->
      (* A server failure is never disguised as a recoverable user
         error. *)
      server_error_page request

(* --- The shared POST implementation ---

   One private handler parameterized by the redirect destination alone.
   After the shared gates: read both route parameters, load the config
   (solely to reuse the same public-origin policy as the other
   project-home mutations), check same-origin, parse the framework-CSRF-
   verified form, require zero application fields, then call the
   transactional removal store in one short SQL scope with no outer
   transaction and no pre-authorization of any kind. *)
let make_removal_handler ~surface ~redirect_of ~mode ~load_config request =
  with_gates ~mode request (fun ~user_id ->
      match
        ( route_param request "project_slug",
          route_param request "community_slug" )
      with
      | None, _ | _, None -> not_found_page request
      | Some project_slug, Some community_slug -> (
          match load_config () with
          | Error _ -> unavailable_page request
          | Ok config -> (
              if not (Request_origin.same_origin_request config request) then
                forbidden_page request
              else
                (* Dream's form API enforces the URL-encoded content type
                   and verifies its own CSRF field, which it strips from
                   the returned fields. Every CSRF failure collapses to one
                   generic 403; no framework diagnostic reaches the
                   response. *)
                match%lwt Dream.form request with
                | `Wrong_content_type -> bad_request_page request
                | `Expired _ | `Wrong_session _ | `Invalid_token _
                | `Missing_token _ | `Many_tokens _ ->
                    forbidden_page request
                | `Ok fields -> (
                    (* The removal form carries no application field: any
                       remaining value (an id, a decision, a return URL, a
                       confirmation, a duplicate, or an unknown key) is a
                       generic 400 that never reaches the store. The route
                       itself expresses removal. *)
                    match fields with
                    | _ :: _ -> bad_request_page request
                    | [] ->
                        let%lwt result =
                          Dream.sql request (fun db ->
                              Store.remove db ~actor_user_id:user_id
                                ~project_slug ~community_slug)
                        in
                        handle_removal_result request ~user_id ~surface
                          ~redirect:(redirect_of ~project_slug ~community_slug)
                          result))))

(* --- POST /projects/:project_slug/community-home/:community_slug/remove --- *)

let make_project_side_home_removal_handler ~mode ~load_config request =
  make_removal_handler ~surface:Analytics.Removal_project_route
    ~redirect_of:(fun ~project_slug ~community_slug:_ () ->
      request_home_redirect ~project_slug)
    ~mode ~load_config request

(* --- POST /c/:community_slug/projects/:project_slug/remove-home --- *)

let make_community_side_home_removal_handler ~mode ~load_config request =
  make_removal_handler ~surface:Analytics.Removal_community_route
    ~redirect_of:(fun ~project_slug:_ ~community_slug () ->
      community_settings_redirect ~community_slug)
    ~mode ~load_config request
