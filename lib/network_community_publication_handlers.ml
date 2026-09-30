(* HTTP layer for the final setup surface of one provisioned network
   community: GET /c/:slug/setup renders the review-and-publish page and
   POST /c/:slug/publish commits it. Both handlers are factories
   over the closed onboarding mode (and, for the POST, a config loader) so
   tests can inject fixed values without touching the process environment,
   and a disabled or rejected request never triggers a route read, a
   configuration read, or SQL.

   Nothing here logs a query or a form value, and no submitted value — name,
   slug, description, or publication choice — is ever placed in a URL,
   cookie, or log: failed submissions re-render over POST instead of
   redirecting, and the one success redirect is built structurally from the
   canonical slug the store committed. *)

module Read = Network_community_publication_read_model
module Form = Network_community_publication_form
module Store = Network_community_publication_store
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
   never cacheable, and never leaking onward as a cross-origin Referer. The
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

(* The one post-publication destination: the community's own public home,
   built structurally from the canonical slug the committed transaction
   returned — never the route's pre-publication slug, and never with a
   query, fragment, or result token. /settings is deliberately not the
   destination: the product result of publishing is the now-public
   community. *)
let community_redirect ~community_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make ~path:(String.concat "/" [ ""; "c"; community_slug ]) ()))

(* Rejected before the form is even parsed: no submitted value or framework
   diagnostic reaches either response. *)
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

(* Configuration-loading failure on the POST: a deployment problem, answered
   generically and never disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:
         "Publishing this community is temporarily unavailable. Please try \
          again later."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 404 for every unavailable community — nonexistent, legacy,
   already published, and every unauthorized identity stay
   indistinguishable, and no community name or lifecycle state leaks through
   it. Byte-identical to the sibling project-home 404s. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

(* One generic 500 for every read-model failure past the gates — no
   Caqti/PostgreSQL detail, error constructor, or fixture value ever reaches
   the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Error"
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

(* --- Safe form-value preservation ---

   The rejected submission's own four values, returned only when the field
   set is provably exactly one occurrence of each expected name and nothing
   else — the same structural shape the parser accepts. Any other shape (an
   unknown key, a duplicate, a missing field) yields blanks, so a structural
   rejection reflects nothing a client planted. Values are never trimmed,
   canonicalized, or repaired here; the page escapes them at the template
   boundary. Dream has already stripped its own CSRF field, so it can never
   appear among these. *)
let blank_values : Pages_ncp.form_values =
  {
    Pages_ncp.community_name = "";
    community_slug = "";
    community_description = "";
    publication_visibility = "";
  }

let submitted_values fields : Pages_ncp.form_values =
  let exactly_once name =
    match List.filter (fun (key, _) -> String.equal key name) fields with
    | [ (_, value) ] -> Some value
    | _ -> None
  in
  if List.length fields <> 4 then blank_values
  else
    match
      ( exactly_once "community_name",
        exactly_once "community_slug",
        exactly_once "community_description",
        exactly_once "publication_visibility" )
    with
    | Some name, Some slug, Some description, Some visibility ->
        {
          Pages_ncp.community_name = name;
          community_slug = slug;
          community_description = description;
          publication_visibility = visibility;
        }
    | _ -> blank_values

(* --- Owner-authorized read-and-render ---

   One helper shared by the GET and every recoverable POST failure: the
   community is always re-authorized fresh in its own short Dream.sql scope —
   never reused across the store call — so a lost top_mod role, a revoked
   durable admin, a concurrently published draft, and a removed home relation
   all take effect before anything private renders again. The live request
   crosses so the rendered form carries a fresh Dream CSRF field, and
   [values_of] lets the GET prefill from the draft's own identity while the
   POST reflects the submission it just rejected. *)
let respond_owner_authorized request ~user_id ~community_slug ~values_of
    ~feedback ~status =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Read.load_for_publisher db ~user_id ~community_slug)
  in
  match loaded with
  (* Nonexistent, legacy, published, and every unauthorized identity stay
     indistinguishable, and a malformed slug answers identically. *)
  | Ok None | Error Read.Invalid_community_slug -> not_found_page request
  | Error Read.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Read.Inconsistent_data | Read.Storage_error) ->
      server_error_page request
  | Ok (Some view) ->
      let community = Read.community view in
      Dream.respond ~status ~headers:page_headers
        (Pages_ncp.network_community_publication_page
           ?user:(session_user request) ~request
           ~community:(community_of_read community)
           ~project:(project_of_read (Read.project view))
           ~values:(values_of community) ~feedback ())

(* --- GET /c/:slug/setup --- *)

let make_network_community_publication_page_handler ~mode request =
  with_gates ~mode request (fun ~user_id ->
      match route_slug request with
      | None -> not_found_page request
      | Some slug ->
          respond_owner_authorized request ~user_id ~community_slug:slug
            ~values_of:values_of_community ~feedback:None ~status:`OK)

(* --- Store-result mapping --- *)

let handle_store_result request ~user_id ~slug ~values = function
  | Ok published -> (
      match Store.publication_visibility published with
      (* The closed result vocabulary: a committed publication is always
         reachable, and there is no Private or draft shape to represent. The
         choice itself never changes the destination. *)
      | (Form.Public | Form.Unlisted) as visibility ->
          (* The publication transaction committed as Public or Unlisted:
             the one publication boundary. The store's own closed value is
             mapped exhaustively onto the analytics vocabulary, so a new
             domain constructor breaks the build here rather than degrading
             silently, and Private stays unrepresentable on both sides. The
             final canonical slug — the community's real identity, never the
             obsolete route slug — is used for the redirect only; no
             community identity crosses into analytics, because the store's
             narrow result carries no community id. *)
          Analytics.capture_if_consented request
            ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
            (Analytics.Network_community_published
               {
                 user_id;
                 publication_visibility =
                   (match visibility with
                   | Form.Public -> Analytics.Published_public
                   | Form.Unlisted -> Analytics.Published_unlisted);
               });
          (* PRG into the now-published community. The canonical slug comes
             only from the store, so a publication that moved the slug lands
             on the new one; no submitted value, id, or result token enters
             the URL. *)
          Lwt.return
            (community_redirect
               ~community_slug:(Store.community_slug published)))
  | Error Store.Community_slug_unavailable ->
      (* Recoverable: the failed transaction left the draft under its
         original slug, so the route slug still addresses it and the
         publisher can edit the requested address and retry. Nothing names
         the conflicting community's id, owner, lifecycle, visibility, or
         legacy/network classification. *)
      respond_owner_authorized request ~user_id ~community_slug:slug
        ~values_of:(fun _ -> values)
        ~feedback:(Some Pages_ncp.Community_slug_unavailable)
        ~status:`Conflict
  | Error Store.Draft_unavailable ->
      (* Missing, legacy, already published (including a replayed
         submission), renamed, unauthorized, concurrently demoted, and a
         relation removed first all collapse to the one generic 404 — never
         a re-rendered stale draft and never a guessed current slug. *)
      not_found_page request
  | Error Store.Invalid_community_slug ->
      (* Defensive: a malformed route slug is the same generic 404, never
         reflected. *)
      not_found_page request
  | Error Store.Invalid_user_id ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Store.Inconsistent_data | Store.Storage_error) ->
      (* A server failure is never disguised as a recoverable user error. *)
      server_error_page request

(* --- POST /c/:slug/publish --- *)

let make_network_community_publication_handler ~mode ~load_config request =
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
                   application fields only.

                   Every CSRF failure still refuses the submission with 403
                   and never opens the store, but it is answered with the
                   owner-authorized page rather than a terminal message
                   page: the framework token lives one hour while the
                   session that renders it lives two weeks, so a setup page
                   left open (or served before a restart, which rotates the
                   encryption secret) would otherwise become permanently
                   unsubmittable. The re-render is already past the session,
                   rollout and same-origin gates, re-authorizes the
                   publisher in SQL, and reflects nothing from the
                   unverified submission. *)
                match%lwt Dream.form request with
                | `Wrong_content_type -> bad_request_page request
                | `Expired _ | `Wrong_session _ | `Invalid_token _
                | `Missing_token _ | `Many_tokens _ ->
                    respond_owner_authorized request ~user_id
                      ~community_slug:slug
                      ~values_of:(fun _ -> blank_values)
                      ~feedback:(Some Pages_ncp.Stale_form) ~status:`Forbidden
                | `Ok fields -> (
                    (* No store SQL opens before the submission is
                       structurally and semantically valid; the parser owns
                       the whole grammar, and nothing is trimmed, repaired,
                       or re-derived here. *)
                    let reject feedback ~values =
                      respond_owner_authorized request ~user_id
                        ~community_slug:slug
                        ~values_of:(fun _ -> values)
                        ~feedback:(Some feedback) ~status:(`Status 422)
                    in
                    match Form.of_fields fields with
                    | Error Form.Invalid_form ->
                        (* Structurally invalid: the field set cannot be
                           proven safe, so nothing at all is reflected. *)
                        reject Pages_ncp.Invalid_form ~values:blank_values
                    | Error Form.Invalid_community_name ->
                        reject Pages_ncp.Invalid_community_name
                          ~values:(submitted_values fields)
                    | Error Form.Invalid_community_slug ->
                        reject Pages_ncp.Invalid_community_slug
                          ~values:(submitted_values fields)
                    | Error Form.Invalid_community_description ->
                        reject Pages_ncp.Invalid_community_description
                          ~values:(submitted_values fields)
                    | Error Form.Invalid_publication_visibility ->
                        reject Pages_ncp.Invalid_publication_visibility
                          ~values:(submitted_values fields)
                    | Ok publication ->
                        (* One short Dream.sql scope and no outer
                           transaction: the store owns its own transaction,
                           locking, authorization, lifecycle transition, slug
                           arbitration, verification, commit, and rollback.
                           Nothing is pre-checked here. *)
                        let%lwt result =
                          Dream.sql request (fun db ->
                              Store.publish db ~actor_user_id:user_id
                                ~current_community_slug:slug ~publication)
                        in
                        handle_store_result request ~user_id ~slug
                          ~values:(submitted_values fields) result))))
