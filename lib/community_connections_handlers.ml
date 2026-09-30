(* HTTP layer for the community-connections management workflow.

   The shape of every handler is the same: session gate, route community,
   authorized read-model load (which is where top_mod-or-durable-admin is
   decided, in SQL), then — for the mutations — CSRF, subject binding against
   the freshly loaded connection, and only then the transactional store.
   Authorization never happens after a mutation, and a hidden or route id
   never establishes authority: every subject is re-resolved server-side.

   Every unauthorized or unavailable outcome collapses into one generic 404
   so no slug or id becomes an existence oracle. See the .mli for the full
   contract. *)

module Read = Community_connections_management_read_model
module Rows = Community_connections_read_model
module Store = Community_connections_store
module Pages_ccn = Community_connections_pages
module Cc = Community_connections

(* --- Session reads (identical rules to the sibling feature handlers) --- *)

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
   without a valid positive session user id, and even then it only enables
   the durable users.is_admin check inside the read model's SQL. *)
let authenticated_user_id request =
  match session_field_opt request "user_id" with
  | None -> None
  | Some raw -> (
      match int_of_string_opt raw with
      | Some id when id > 0 -> Some id
      | _ -> None)

let session_global_admin request =
  session_field_opt request "is_admin" = Some "true"

(* --- Responses --- *)

(* Every rendered page reflects session identity and private workflow state:
   never cacheable, and never leaking onward as a cross-origin Referer. The
   pages stay noindex from the page module itself. *)
let page_headers =
  [
    ("Cache-Control", "no-store");
    ("Referrer-Policy", Request_origin.referrer_policy);
  ]

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

(* Deliberately no return URL: nothing caller-controlled rides along. *)
let login_redirect () = clean_redirect "/login"

(* One generic 404 for every unavailable surface — missing community,
   ordinary member, non-member, 'mod'/'legacy_mod', moderator of another
   community, an unbacked admin claim, a connection belonging to another
   pair, and a malformed route value all stay indistinguishable. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error" ~return_url:"/"
       request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 500 for every read-model or store failure past the gates — no
   Caqti/PostgreSQL detail, error constructor, or durable value reaches the
   page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* Route parameters, absent when the handler runs outside a router (as in the
   DB-free gate tests) — treated defensively like any other unavailable
   value. *)
let route_param request name =
  match Dream.param request name with
  | value -> Some value
  | exception _ -> None

(* The PRG destination is this community's own management page, rebuilt
   structurally from the canonical stored slug — never by reflecting request
   data, and with no connection id, decision, or result state in the URL. *)
let management_redirect ~community_slug =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:
            (String.concat "/"
               [ ""; "c"; community_slug; "settings"; "connections" ])
          ()))

(* --- Read model → page view models (mechanical, public accessors only) --- *)

let counterpart_of_read entry : Pages_ccn.counterpart =
  let c = Read.entry_counterpart entry in
  {
    Pages_ccn.counterpart_name = Read.counterpart_name c;
    counterpart_slug = Read.counterpart_slug c;
  }

let accepted_of_read entry : Pages_ccn.accepted =
  {
    Pages_ccn.accepted_id = Int64.to_string (Read.entry_id entry);
    accepted_with = counterpart_of_read entry;
  }

(* The private note crosses only from the authorized read model to the
   authorized management page; it is never logged and never reflected. *)
let pending_of_read entry : Pages_ccn.pending =
  {
    Pages_ccn.pending_id = Int64.to_string (Read.entry_id entry);
    pending_with = counterpart_of_read entry;
    pending_note = Read.entry_note entry;
  }

let community_of_read view : Pages_ccn.community =
  {
    Pages_ccn.name = Read.view_community_name view;
    slug = Read.view_community_slug view;
    eligible = Read.view_community_eligible view;
  }

let state_of_view view : Pages_ccn.state =
  {
    Pages_ccn.community = community_of_read view;
    accepted = List.map accepted_of_read (Read.view_accepted view);
    incoming = List.map pending_of_read (Read.view_incoming view);
    outgoing = List.map pending_of_read (Read.view_outgoing view);
  }

(* Cartographic Civic shell data for an ALREADY-AUTHORIZED render: the
   durable community record (rail tile, community context), the viewer's
   joined communities in the same stable order the sibling surfaces load, and
   the shared knowledge sidebar with Connections active. Queried only after
   the read model authorized the manager in SQL, so a denied, anonymous, or
   missing-community request never touches membership data and the
   generic-404 collapse is unchanged. Every failure degrades to None — the
   page then falls back to its chrome-free document rather than turning a
   decorative load into a new error path. The sidebar suppresses the
   private-community visibility marker: this surface never names an
   ineligibility reason anywhere in its document. can_manage is true by
   construction — the read model only admits top mods and durable admins. *)
let load_launch_shell request ~user_id ~canonical_slug =
  Dream.sql request (fun db ->
      match%lwt Community_store.get_community_by_slug db canonical_slug with
      | Ok (Some community) ->
          let%lwt channels =
            match%lwt
              Channel_store.get_channels_by_community db community.id
            with
            | Ok channels -> Lwt.return channels
            | Error _ -> Lwt.return []
          in
          let%lwt sections =
            match%lwt
              Section_store.get_sections_by_community db community.id
            with
            | Ok sections -> Lwt.return sections
            | Error _ -> Lwt.return []
          in
          let%lwt rail_communities =
            match%lwt Membership_store.get_user_communities db user_id with
            | Ok communities -> Lwt.return communities
            | Error _ -> Lwt.return []
          in
          let sidebar =
            Community_pages.launch_knowledge_sidebar ~community ~channels
              ~sections ~settings_active:true ~show_visibility_note:false
              ~can_manage:true ()
          in
          Lwt.return (Some (community, rail_communities, sidebar))
      | Ok None | Error _ -> Lwt.return None)

(* --- Shared authorized load ---

   One authorized read-and-render used by the GET and by every recoverable
   POST outcome: the view is always loaded fresh in its own short Dream.sql
   scope — never reused across a store call — so a concurrent winner
   disappears and changed eligibility is current. The live request crosses so
   every rendered form carries a fresh Dream CSRF field. *)
let with_authorized_view request ~user_id ~community_slug k =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Read.load_for_manager db ~user_id
          ~session_global_admin:(session_global_admin request)
          ~community_slug)
  in
  match loaded with
  | Ok None | Error Read.Invalid_community_slug -> not_found_page request
  | Error (Read.Invalid_user_id | Read.Invalid_community_id) ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Read.Inconsistent_data | Read.Storage_error) ->
      server_error_page request
  | Ok (Some view) -> k view

let respond_management request ~user_id ~community_slug ~feedback ~status =
  with_authorized_view request ~user_id ~community_slug (fun view ->
      let%lwt shell =
        load_launch_shell request ~user_id
          ~canonical_slug:(Read.view_community_slug view)
      in
      Dream.respond ~status ~headers:page_headers
        (Pages_ccn.management_page ?user:(session_user request) ~request ?shell
           ~state:(state_of_view view) ~feedback ()))

(* --- Shared session gate ---

   A valid positive session user id, before any route parameter, form read,
   or SQL scope. There is deliberately no rollout mode here: connections are
   an ordinary community capability, not part of the GitHub onboarding
   rollout. *)
let with_session request k =
  match authenticated_user_id request with
  | None -> Lwt.return (login_redirect ())
  | Some user_id -> k ~user_id

let with_community request k =
  match route_param request "slug" with
  | None -> not_found_page request
  | Some community_slug -> k ~community_slug

(* --- GET /c/:slug/settings/connections --- *)

let make_connections_page_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          (* Informational: no locks, no state change, no flash state. *)
          respond_management request ~user_id ~community_slug ~feedback:None
            ~status:`OK))

(* --- GET /c/:slug/settings/connections/new --- *)

(* Step two is entered only when ?target= still names a community the search
   would offer: the confirmation page re-runs the same bounded search over
   the exact slug rather than trusting the query parameter, so a target that
   became ineligible, or that already shares an active connection, falls back
   to step one instead of rendering a form that the store would refuse. *)
let make_connections_search_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_authorized_view request ~user_id ~community_slug (fun view ->
              let page_community = community_of_read view in
              let%lwt shell =
                load_launch_shell request ~user_id
                  ~canonical_slug:(Read.view_community_slug view)
              in
              if not (Read.view_community_eligible view) then
                (* An ineligible community may not create connections at all;
                   the search surface says so rather than offering targets it
                   could never use. *)
                Dream.respond ~status:`Forbidden ~headers:page_headers
                  (Pages_ccn.target_search_page ?user:(session_user request)
                     ~request ?shell ~community:page_community ~query:""
                     ~results:[] ~searched:false
                     ~feedback:(Some Pages_ccn.Source_ineligible) ())
              else
                let query =
                  match Dream.query request "q" with Some q -> q | None -> ""
                in
                let refused feedback =
                  Dream.respond ~status:`OK ~headers:page_headers
                    (Pages_ccn.target_search_page ?user:(session_user request)
                       ~request ?shell ~community:page_community ~query:""
                       ~results:[] ~searched:false ~feedback:(Some feedback) ())
                in
                match Dream.query request "target" with
                | Some slug -> (
                    (* Step two. The exact slug gets an exact resolution — a
                       substring search would only approximate it. *)
                    let%lwt resolved =
                      Dream.sql request (fun db ->
                          Read.resolve_target db
                            ~community_id:(Read.view_community_id view)
                            ~slug)
                    in
                    match resolved with
                    | Error _ -> server_error_page request
                    | Ok Read.Unavailable ->
                        refused Pages_ccn.Target_unavailable
                    | Ok Read.Already_active ->
                        refused Pages_ccn.Already_connected
                    | Ok (Read.Connectable t) ->
                        let note =
                          match Dream.query request "note" with
                          | Some n -> n
                          | None -> ""
                        in
                        Dream.respond ~status:`OK ~headers:page_headers
                          (Pages_ccn.confirm_page ?user:(session_user request)
                             ~request ?shell ~community:page_community
                             ~target:
                               {
                                 Pages_ccn.target_name = Read.target_name t;
                                 target_slug = Read.target_slug t;
                               }
                             ~note ~feedback:None ()))
                | None -> (
                    (* Step one. A blank query never reaches SQL. *)
                    let searched = String.trim query <> "" in
                    let%lwt search =
                      if not searched then Lwt.return (Ok [])
                      else
                        Dream.sql request (fun db ->
                            Read.search_targets db
                              ~community_id:(Read.view_community_id view)
                              ~query)
                    in
                    match search with
                    | Error _ -> server_error_page request
                    | Ok results ->
                        Dream.respond ~status:`OK ~headers:page_headers
                          (Pages_ccn.target_search_page
                             ?user:(session_user request) ~request ?shell
                             ~community:page_community ~query
                             ~results:
                               (List.map
                                  (fun t : Pages_ccn.target ->
                                    {
                                      Pages_ccn.target_name = Read.target_name t;
                                      target_slug = Read.target_slug t;
                                    })
                                  results)
                             ~searched ~feedback:None ())))))

(* --- Shared mutation entry ---

   Past the session gate and the route community: the authorized view (which
   is the authorization decision), then Dream's form API — which enforces the
   URL-encoded content type and verifies its own CSRF field, stripping it
   from the returned fields.

   Every CSRF failure still refuses the submission with 403 and never opens
   the store, but it is answered with the authorized management page rather
   than a terminal message: the framework token lives one hour while the
   session that renders it lives two weeks, so a page left open (or served
   before a restart, which rotates the encryption secret) would otherwise
   become permanently unactionable. The reload is already past the session
   gate, re-authorizes in the read-model SQL, and carries a fresh token; no
   framework diagnostic reaches the response. *)
let with_mutation request k =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_authorized_view request ~user_id ~community_slug (fun view ->
              match%lwt Dream.form request with
              | `Wrong_content_type -> bad_request_page request
              | `Expired _ | `Wrong_session _ | `Invalid_token _
              | `Missing_token _ | `Many_tokens _ ->
                  respond_management request ~user_id ~community_slug
                    ~feedback:(Some Pages_ccn.Stale_form) ~status:`Forbidden
              | `Ok fields -> k ~user_id ~community_slug ~view ~fields)))

(* --- POST /c/:slug/settings/connections/request --- *)

let request_result request ~user_id ~community_slug = function
  | Ok _ -> Lwt.return (management_redirect ~community_slug)
  | Error Store.Active_connection_exists ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Already_connected) ~status:`Conflict
  | Error Store.Requester_ineligible ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Source_ineligible) ~status:`Conflict
  | Error (Store.Community_unavailable | Store.Recipient_ineligible) ->
      (* Missing, private, draft, and undiscoverable collapse into one
         answer: the target's state never crosses. *)
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Target_unavailable) ~status:`Conflict
  | Error
      ( Store.Invalid_user_id | Store.Invalid_connection_id
      | Store.Invalid_community_id | Store.Invalid_connection
      | Store.Review_unavailable | Store.Removal_unavailable
      | Store.Inconsistent_data | Store.Storage_error ) ->
      (* Defensive: the gates and the pure constructor already excluded
         these. A server failure is never disguised as a recoverable one. *)
      server_error_page request

let make_connection_request_handler request =
  with_mutation request (fun ~user_id ~community_slug ~view ~fields ->
      let field name = List.assoc_opt name fields in
      match (field "target", List.length fields) with
      (* Exactly the two application fields this form emits; a third value, a
         duplicate, or an unknown key is a generic 400 that never reaches the
         store. *)
      | Some target_slug, n when n <= 2 -> (
          let note = match field "note" with Some n -> n | None -> "" in
          if not (Read.view_community_eligible view) then
            respond_management request ~user_id ~community_slug
              ~feedback:(Some Pages_ccn.Source_ineligible) ~status:`Conflict
          else
            (* The target is re-resolved server-side by its exact slug —
               the posted value identifies a record and grants nothing. *)
            let%lwt resolved =
              Dream.sql request (fun db ->
                  Read.resolve_target db
                    ~community_id:(Read.view_community_id view)
                    ~slug:target_slug)
            in
            match resolved with
            | Error _ -> server_error_page request
            | Ok resolution -> (
                match resolution with
                | Read.Unavailable ->
                    respond_management request ~user_id ~community_slug
                      ~feedback:(Some Pages_ccn.Target_unavailable)
                      ~status:`Conflict
                | Read.Already_active ->
                    respond_management request ~user_id ~community_slug
                      ~feedback:(Some Pages_ccn.Already_connected)
                      ~status:`Conflict
                | Read.Connectable t -> (
                    match
                      Cc.create_pending
                        ~requester_community_id:(Read.view_community_id view)
                        ~recipient_community_id:(Read.target_id t)
                        ~request_note:(Some note)
                    with
                    | Error Cc.Invalid_request_note ->
                        respond_management request ~user_id ~community_slug
                          ~feedback:(Some Pages_ccn.Note_invalid)
                          ~status:`Bad_Request
                    | Error (Cc.Invalid_community_id | Cc.Self_connection) ->
                        (* The search already excluded this community
                           itself; anything else here is a broken
                           invariant. *)
                        respond_management request ~user_id ~community_slug
                          ~feedback:(Some Pages_ccn.Target_unavailable)
                          ~status:`Conflict
                    | Error Cc.Invalid_transition -> server_error_page request
                    | Ok connection ->
                        let%lwt result =
                          Dream.sql request (fun db ->
                              Store.request db ~actor_user_id:user_id
                                ~connection)
                        in
                        request_result request ~user_id ~community_slug result))
          )
      | _ -> bad_request_page request)

(* --- Shared subject binding for the three id-addressed mutations ---

   The connection is re-read server-side from its own row and checked against
   the route community. The route id identifies the record; the route
   community decides authority, and the two must agree before the store is
   opened. Every mismatch is the same generic 404. *)
let with_bound_connection request ~fields ~view k =
  match (route_param request "id", fields) with
  | None, _ -> not_found_page request
  (* These forms carry no application field at all. *)
  | Some _, _ :: _ -> bad_request_page request
  | Some raw_id, [] -> (
      match Int64.of_string_opt raw_id with
      | None -> not_found_page request
      | Some connection_id when Int64.compare connection_id 0L <= 0 ->
          not_found_page request
      | Some connection_id -> (
          let%lwt loaded =
            Dream.sql request (fun db -> Rows.load db ~connection_id)
          in
          match loaded with
          | Error Rows.Invalid_connection_id -> not_found_page request
          | Error
              ( Rows.Invalid_community_id | Rows.Inconsistent_data
              | Rows.Storage_error ) ->
              server_error_page request
          | Ok None -> not_found_page request
          | Ok (Some row) ->
              let community_id = Read.view_community_id view in
              if
                row.Rows.requester_community_id <> community_id
                && row.Rows.recipient_community_id <> community_id
              then
                (* The route community is not part of this connection: the
                   id names someone else's record. *)
                not_found_page request
              else k ~connection_id ~row))

(* --- POST /c/:slug/settings/connections/:id/{accept,reject} --- *)

let review_result request ~user_id ~community_slug ~decision = function
  | Ok reviewed ->
      let expected =
        match decision with
        | Store.Accept -> Cc.Accepted
        | Store.Reject -> Cc.Rejected
      in
      if Store.reviewed_status reviewed <> expected then
        (* A decision/result mismatch is a broken invariant, not a public
           state. *)
        server_error_page request
      else Lwt.return (management_redirect ~community_slug)
  | Error Store.Review_unavailable ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Review_unavailable) ~status:`Conflict
  | Error Store.Recipient_ineligible ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Source_ineligible) ~status:`Conflict
  | Error (Store.Community_unavailable | Store.Requester_ineligible) ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Target_unavailable) ~status:`Conflict
  | Error
      ( Store.Invalid_user_id | Store.Invalid_connection_id
      | Store.Invalid_community_id | Store.Invalid_connection
      | Store.Active_connection_exists | Store.Removal_unavailable
      | Store.Inconsistent_data | Store.Storage_error ) ->
      server_error_page request

let make_review_handler ~decision request =
  with_mutation request (fun ~user_id ~community_slug ~view ~fields ->
      with_bound_connection request ~fields ~view (fun ~connection_id ~row ->
          let community_id = Read.view_community_id view in
          (* Accept and reject belong to the recipient alone: the requesting
             side reviewing its own request is refused here, before the
             store, and again by the store's guarded mutation. *)
          if row.Rows.recipient_community_id <> community_id then
            not_found_page request
          else
            let%lwt result =
              Dream.sql request (fun db ->
                  Store.review db ~reviewer_user_id:user_id ~connection_id
                    ~recipient_community_id:community_id ~decision)
            in
            review_result request ~user_id ~community_slug ~decision result))

let make_connection_accept_handler request =
  make_review_handler ~decision:Store.Accept request

let make_connection_reject_handler request =
  make_review_handler ~decision:Store.Reject request

(* --- POST /c/:slug/settings/connections/:id/remove --- *)

let removal_result request ~user_id ~community_slug = function
  | Ok _ -> Lwt.return (management_redirect ~community_slug)
  | Error Store.Removal_unavailable ->
      respond_management request ~user_id ~community_slug
        ~feedback:(Some Pages_ccn.Removal_unavailable) ~status:`Conflict
  | Error
      ( Store.Invalid_user_id | Store.Invalid_connection_id
      | Store.Invalid_community_id | Store.Invalid_connection
      | Store.Community_unavailable | Store.Requester_ineligible
      | Store.Recipient_ineligible | Store.Active_connection_exists
      | Store.Review_unavailable | Store.Inconsistent_data | Store.Storage_error
        ) ->
      server_error_page request

let make_connection_removal_handler request =
  with_mutation request (fun ~user_id ~community_slug ~view ~fields ->
      with_bound_connection request ~fields ~view (fun ~connection_id ~row:_ ->
          (* Either side may remove, so the acting community is the route
             community — already proved to be one of the pair — and it is
             what the store's guarded mutation verifies. *)
          let%lwt result =
            Dream.sql request (fun db ->
                Store.remove db ~actor_user_id:user_id ~connection_id
                  ~acting_community_id:(Read.view_community_id view))
          in
          removal_result request ~user_id ~community_slug result))
