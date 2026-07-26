(* HTTP layer for the final setup surface of one provisioned network
   community (GET /c/:slug/setup), kept out of the legacy Handlers
   macro-module per the feature-module guideline. The handler is a factory
   over the closed onboarding mode so tests can inject a fixed value without
   touching the process environment, and a disabled or rejected request never
   triggers a route read or SQL.

   The page's form names POST /c/:slug/publish, which this slice does not
   register: the publication store does not exist yet, and no legacy route
   may stand in for it (see the guards in Handlers). Nothing here logs a
   query and nothing travels in a query string, cookie, or flash message. *)

module Read = Network_community_publication_read_model
module Pages_ncp = Network_community_publication_pages

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
   without a valid positive session user id, and never authorizes this page:
   only the read model's durable users.is_admin does. *)
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

(* One generic 404 for every unavailable community — nonexistent, legacy,
   already published, and every unauthorized identity stay
   indistinguishable, and no community name or lifecycle state leaks through
   it. Byte-identical to the sibling project-home 404s. *)
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
   the DB-free gate tests) — treated like any other unavailable community.
   Never trimmed, lowercased, repaired, or percent-decoded by hand: the read
   model owns validation. *)
let route_slug request =
  match Dream.param request "slug" with
  | value -> Some value
  | exception _ -> None

(* --- Shared rollout and authentication gates ---

   Identical order and semantics to the other GitHub-project handlers: mode
   Off first (kill switch, no route read, no SQL), then a valid positive
   session user id, then Admins/Public rollout eligibility — all before any
   route parameter read or SQL scope. *)
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

let community_of_read community : Pages_ncp.community =
  {
    Pages_ncp.name = Read.community_name community;
    slug = Read.community_slug community;
    description = Read.community_description community;
  }

let project_of_read project : Pages_ncp.project =
  {
    Pages_ncp.name = Read.project_name project;
    slug = Read.project_slug project;
    namespace_login = Read.project_namespace_login project;
    kind = Read.project_kind project;
  }

(* The first view: the controls carry the draft's current canonical identity
   verbatim, with [public] as the initial publication choice. An absent
   description is an empty textarea, not the string "None". *)
let values_of_community community : Pages_ncp.form_values =
  {
    Pages_ncp.community_name = Read.community_name community;
    community_slug = Read.community_slug community;
    community_description =
      (match Read.community_description community with
      | Some text -> text
      | None -> "");
    publication_visibility = "public";
  }

(* --- GET /c/:slug/setup --- *)

let make_network_community_publication_page_handler ~mode request =
  with_gates ~mode request (fun ~user_id ->
      match route_slug request with
      | None -> not_found_page request
      | Some slug -> (
          let%lwt loaded =
            Dream.sql request (fun db ->
                Read.load_for_publisher db ~user_id ~community_slug:slug)
          in
          match loaded with
          (* Nonexistent, legacy, published, and every unauthorized identity
             stay indistinguishable, and a malformed slug answers
             identically. *)
          | Ok None | Error Read.Invalid_community_slug -> not_found_page request
          | Error Read.Invalid_user_id ->
              (* Defensive: the session gate already validated the id. *)
              server_error_page request
          | Error (Read.Inconsistent_data | Read.Storage_error) ->
              server_error_page request
          | Ok (Some view) ->
              let community = Read.community view in
              Dream.respond ~status:`OK ~headers:page_headers
                (Pages_ncp.network_community_publication_page
                   ?user:(session_user request) ~request
                   ~community:(community_of_read community)
                   ~project:(project_of_read (Read.project view))
                   ~values:(values_of_community community) ~feedback:None ())))
