(* HTTP layer for the shared-threads workflow. Kept out of the legacy
   Handlers macro-module per the feature-module guideline.

   The shape of every handler is the same: session gate, route subjects,
   authorized read-model load (which is where the policy — author-while-
   member / origin top_mod / durable admin for sharing, top_mod-or-admin
   for management, requester-or-origin-top_mod-or-admin for withdrawal —
   is decided, in SQL), then — for the mutations — CSRF, subject binding
   against the freshly loaded placement, and only then the Slice 1
   transactional store, which stays the only writer and re-verifies
   everything under its own locks. Authorization never happens after a
   mutation, and a hidden or route id never establishes authority.

   Every unauthorized or unavailable outcome collapses into one generic
   404 so no slug, thread id, or placement id becomes an existence oracle.
   See the .mli for the full contract. *)

module Share = Shared_thread_placement_read_model
module Manage = Shared_thread_placement_management_read_model
module Store = Shared_thread_placement_store
module Pages_sth = Shared_thread_placement_pages

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
   negative reads as "not logged in". A session is_admin claim has no
   meaning without a valid positive session user id, and even then it only
   enables the durable users.is_admin check inside the read models' SQL. *)
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

(* Every rendered page reflects session identity and private workflow
   state: never cacheable, and never leaking onward as a cross-origin
   Referer. The pages stay noindex from the page module itself. *)
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

(* Deliberately no return URL: nothing caller-controlled rides along. *)
let login_redirect () = clean_redirect "/login"

(* One generic 404 for every unavailable surface — missing community,
   missing or tombstoned thread, departed or banned author, ordinary
   member, 'mod'/'legacy_mod', moderator of another community, an unbacked
   admin claim, a placement belonging to another community, and a malformed
   route value all stay indistinguishable. *)
let not_found_page request =
  Dream.respond ~status:`Not_Found ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Not Found"
       ~message:"This page does not exist." ~alert_type:"error"
       ~return_url:"/" request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* One generic 500 for every read-model or store failure past the gates —
   no Caqti/PostgreSQL detail, error constructor, or durable value reaches
   the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* Route parameters, absent when the handler runs outside a router (as in
   the DB-free gate tests) — treated defensively like any other unavailable
   value. *)
let route_param request name =
  match Dream.param request name with
  | value -> Some value
  | exception _ -> None

(* --- Redirect destinations ---

   Every destination is rebuilt structurally from canonical stored values —
   never by reflecting request data — and carries only a closed ?done=
   verb the GET maps onto a fixed notice. *)

let management_path ~community_slug =
  String.concat "/" [ ""; "c"; community_slug; "settings"; "shared-threads" ]

let management_redirect ~community_slug ~outcome =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:(management_path ~community_slug)
          ~query:[ ("done", [ outcome ]) ]
          ()))

(* The Share page keyed by the bare post id: the GET accepts the canonical
   thread segment with or without its descriptive slug, so no title is
   needed to build a safe return path. *)
let share_redirect ~origin_slug ~post_id ~outcome =
  clean_redirect
    (Uri.to_string
       (Uri.make
          ~path:
            (String.concat "/"
               [ ""; "c"; origin_slug; "t"; string_of_int post_id; "share" ])
          ~query:[ ("done", [ outcome ]) ]
          ()))

(* The closed ?done= vocabularies. An unknown value renders nothing. *)
let share_notice request =
  match Dream.query request "done" with
  | Some "requested" -> Some Pages_sth.Request_sent
  | Some "withdrawn" -> Some Pages_sth.Request_withdrawn
  | Some "removed" -> Some Pages_sth.Placement_removed
  | _ -> None

let management_notice request =
  match Dream.query request "done" with
  | Some "accepted" -> Some Pages_sth.Request_accepted
  | Some "rejected" -> Some Pages_sth.Request_rejected
  | Some "withdrawn" -> Some Pages_sth.Request_withdrawn
  | Some "removed" -> Some Pages_sth.Placement_removed
  | _ -> None

(* --- The canonical thread segment ---

   ":thread" is "<post_id>-<descriptive-slug>"; the leading integer is
   authoritative, exactly as the thread page itself parses it. The
   descriptive part is never compared here: the Share page is an authorized
   working surface keyed by the post id, and every link it emits is
   canonical. *)
let post_id_of_thread_param raw =
  let head =
    match String.index_opt raw '-' with
    | Some i -> String.sub raw 0 i
    | None -> raw
  in
  match int_of_string_opt head with
  | Some id when id > 0 -> Some id
  | _ -> None

(* --- Read model → page view models (mechanical, public accessors only) --- *)

let share_state_of_view view : Pages_sth.share_state =
  let origin_slug = Share.view_origin_community_slug view in
  let manager = Share.view_origin_manager view in
  {
    Pages_sth.share_thread_title = Share.view_post_title view;
    share_origin_name = Share.view_origin_community_name view;
    share_origin_slug = origin_slug;
    share_thread_path =
      Components.canonical_thread_path origin_slug (Share.view_post_id view)
        (Share.view_post_title view);
    share_candidates =
      List.map
        (fun c ->
          {
            Pages_sth.candidate_name = Share.candidate_name c;
            candidate_slug = Share.candidate_slug c;
          })
        (Share.view_candidates view);
    share_placements =
      List.map
        (fun p ->
          {
            Pages_sth.share_placement_id =
              Int64.to_string (Share.placement_id p);
            share_destination_name = Share.placement_destination_name p;
            share_destination_slug = Share.placement_destination_slug p;
            share_pending = Share.placement_pending p;
            (* Control gates only: the withdraw and remove routes
               re-authorize from scratch. *)
            share_can_withdraw =
              manager || Share.placement_requested_by_viewer p;
            share_can_remove = manager;
            share_note = Share.placement_note p;
          })
        (Share.view_placements view);
    share_manage_connections = manager;
  }

let pending_entry_of_read row : Pages_sth.pending_entry =
  let counterpart = Manage.pending_counterpart row in
  {
    Pages_sth.pending_id = Int64.to_string (Manage.pending_placement_id row);
    pending_title = Manage.pending_post_title row;
    pending_thread_path =
      Components.canonical_thread_path
        (Manage.counterpart_slug counterpart)
        (Manage.pending_post_id row)
        (Manage.pending_post_title row);
    pending_counterpart_name = Manage.counterpart_name counterpart;
    pending_counterpart_slug = Manage.counterpart_slug counterpart;
    pending_note = Manage.pending_note row;
    pending_requested_at = Manage.pending_requested_at row;
  }

(* An outgoing pending row's thread lives in THIS community; the incoming
   one's lives in the counterpart. The accepted rows mirror the same
   split. *)
let pending_entry_of_read_own ~community_slug row : Pages_sth.pending_entry =
  let counterpart = Manage.pending_counterpart row in
  {
    Pages_sth.pending_id = Int64.to_string (Manage.pending_placement_id row);
    pending_title = Manage.pending_post_title row;
    pending_thread_path =
      Components.canonical_thread_path community_slug
        (Manage.pending_post_id row)
        (Manage.pending_post_title row);
    pending_counterpart_name = Manage.counterpart_name counterpart;
    pending_counterpart_slug = Manage.counterpart_slug counterpart;
    pending_note = Manage.pending_note row;
    pending_requested_at = Manage.pending_requested_at row;
  }

let accepted_entry_of_read ~thread_slug row : Pages_sth.accepted_entry =
  let counterpart = Manage.accepted_counterpart row in
  {
    Pages_sth.accepted_id = Int64.to_string (Manage.accepted_placement_id row);
    accepted_title = Manage.accepted_post_title row;
    accepted_thread_path =
      Components.canonical_thread_path thread_slug
        (Manage.accepted_post_id row)
        (Manage.accepted_post_title row);
    accepted_counterpart_name = Manage.counterpart_name counterpart;
    accepted_counterpart_slug = Manage.counterpart_slug counterpart;
    accepted_section = Manage.accepted_section_name row;
    accepted_at = Manage.accepted_at row;
  }

let management_state_of_view view : Pages_sth.management_state =
  let community_slug = Manage.view_community_slug view in
  {
    Pages_sth.community_name = Manage.view_community_name view;
    community_slug;
    community_eligible = Manage.view_community_eligible view;
    sections_enabled = Manage.view_sections_enabled view;
    section_options =
      List.map
        (fun s ->
          {
            Pages_sth.section_id =
              string_of_int (Manage.section_option_id s);
            section_name = Manage.section_option_name s;
          })
        (Manage.view_section_options view);
    incoming = List.map pending_entry_of_read (Manage.view_incoming view);
    outgoing =
      List.map
        (pending_entry_of_read_own ~community_slug)
        (Manage.view_outgoing view);
    shared_into =
      List.map
        (fun row ->
          accepted_entry_of_read
            ~thread_slug:
              (Manage.counterpart_slug (Manage.accepted_counterpart row))
            row)
        (Manage.view_shared_into view);
    shared_from =
      List.map
        (accepted_entry_of_read ~thread_slug:community_slug)
        (Manage.view_shared_from view);
  }

(* Cartographic Civic shell data for an ALREADY-AUTHORIZED render, identical
   to the connections surface: queried only after the read model authorized
   the viewer in SQL, so a denied, anonymous, or missing-subject request
   never touches membership data and the generic-404 collapse is unchanged.
   Every failure degrades to None — the page then falls back to its
   chrome-free document. *)
let load_launch_shell request ~user_id ~canonical_slug ~can_manage =
  Dream.sql request (fun db ->
      match%lwt Db.get_community_by_slug db canonical_slug with
      | Ok (Some community) ->
          let%lwt channels =
            match%lwt Db.get_channels_by_community db community.Db.id with
            | Ok channels -> Lwt.return channels
            | Error _ -> Lwt.return []
          in
          let%lwt sections =
            match%lwt Db.get_sections_by_community db community.Db.id with
            | Ok sections -> Lwt.return sections
            | Error _ -> Lwt.return []
          in
          let%lwt rail_communities =
            match%lwt Db.get_user_communities db user_id with
            | Ok communities -> Lwt.return communities
            | Error _ -> Lwt.return []
          in
          let sidebar =
            Pages.launch_knowledge_sidebar ~community ~channels ~sections
              ~show_visibility_note:false ~can_manage ()
          in
          Lwt.return (Some (community, rail_communities, sidebar))
      | Ok None | Error _ -> Lwt.return None)

(* --- Shared gates --- *)

(* A valid positive session user id, before any route parameter, form read,
   or SQL scope. *)
let with_session request k =
  match authenticated_user_id request with
  | None -> Lwt.return (login_redirect ())
  | Some user_id -> k ~user_id

let with_community request k =
  match route_param request "slug" with
  | None -> not_found_page request
  | Some community_slug -> k ~community_slug

let with_thread request k =
  match route_param request "thread" with
  | None -> not_found_page request
  | Some raw -> (
      match post_id_of_thread_param raw with
      | None -> not_found_page request
      | Some post_id -> k ~post_id)

let with_placement_id request k =
  match route_param request "placement_id" with
  | None -> not_found_page request
  | Some raw -> (
      match Int64.of_string_opt raw with
      | None -> not_found_page request
      | Some placement_id when Int64.compare placement_id 0L <= 0 ->
          not_found_page request
      | Some placement_id -> k ~placement_id)

(* --- The Share surface --- *)

(* One authorized read-and-render used by the GET and by every recoverable
   POST outcome: the view is always loaded fresh in its own short Dream.sql
   scope — never reused across a store call — so a concurrent winner
   disappears and changed authority is current. The route community must
   equal the post's immutable origin; a mismatch answers exactly like a
   missing thread. *)
let with_share_view request ~user_id ~community_slug ~post_id k =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Share.load_share_view db ~user_id
          ~session_global_admin:(session_global_admin request)
          ~post_id)
  in
  match loaded with
  | Ok None -> not_found_page request
  | Error (Share.Invalid_user_id | Share.Invalid_post_id) ->
      (* Defensive: the session and route gates already validated both. *)
      server_error_page request
  | Error (Share.Inconsistent_data | Share.Storage_error) ->
      server_error_page request
  | Ok (Some view) ->
      if
        not
          (String.equal (Share.view_origin_community_slug view) community_slug)
      then not_found_page request
      else k view

let respond_share request ~user_id ~community_slug ~post_id ~notice ~feedback
    ~status =
  with_share_view request ~user_id ~community_slug ~post_id (fun view ->
      let%lwt shell =
        load_launch_shell request ~user_id
          ~canonical_slug:(Share.view_origin_community_slug view)
          ~can_manage:(Share.view_origin_manager view)
      in
      Dream.respond ~status ~headers:page_headers
        (Pages_sth.share_page ?user:(session_user request) ~request ?shell
           ~state:(share_state_of_view view) ~notice ~feedback ()))

(* GET /c/:slug/t/:thread/share *)
let make_share_page_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_thread request (fun ~post_id ->
              respond_share request ~user_id ~community_slug ~post_id
                ~notice:(share_notice request) ~feedback:None ~status:`OK)))

(* POST /c/:slug/t/:thread/share *)

let share_request_result request ~user_id ~community_slug ~post_id ~view =
  let respond feedback status =
    respond_share request ~user_id ~community_slug ~post_id ~notice:None
      ~feedback:(Some feedback) ~status
  in
  function
  | Ok _ ->
      Lwt.return
        (clean_redirect
           (Uri.to_string
              (Uri.make
                 ~path:
                   (Components.canonical_thread_path
                      (Share.view_origin_community_slug view)
                      post_id
                      (Share.view_post_title view)
                   ^ "/share")
                 ~query:[ ("done", [ "requested" ]) ]
                 ())))
  | Error Store.Active_placement_exists -> respond Pages_sth.Already_shared `Conflict
  | Error Store.Invalid_request_note -> respond Pages_sth.Note_invalid `Bad_Request
  | Error Store.Origin_ineligible -> respond Pages_sth.Source_ineligible `Conflict
  | Error
      ( Store.Same_community | Store.Community_unavailable
      | Store.No_accepted_connection | Store.Destination_ineligible ) ->
      (* Missing, private, draft, undiscoverable, unconnected, and the
         origin itself collapse into one answer: the destination's state
         never crosses. *)
      respond Pages_sth.Destination_unavailable `Conflict
  | Error (Store.Post_unavailable | Store.Post_tombstoned) ->
      respond Pages_sth.Thread_unavailable `Conflict
  | Error
      ( Store.Invalid_user_id | Store.Invalid_placement_id
      | Store.Invalid_post_id | Store.Invalid_community_id
      | Store.Invalid_destination_section | Store.Review_unavailable
      | Store.Withdrawal_unavailable | Store.Removal_unavailable
      | Store.Inconsistent_data | Store.Storage_error ) ->
      (* Defensive: the gates and the pure constructor already excluded
         these. A server failure is never disguised as a recoverable one. *)
      server_error_page request

let make_share_request_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_thread request (fun ~post_id ->
              with_share_view request ~user_id ~community_slug ~post_id
                (fun view ->
                  match%lwt Dream.form request with
                  | `Wrong_content_type -> bad_request_page request
                  | `Expired _ | `Wrong_session _ | `Invalid_token _
                  | `Missing_token _ | `Many_tokens _ ->
                      (* Refused with a fresh authorized re-render rather
                         than a dead end: the one-hour token lives inside a
                         two-week session. *)
                      respond_share request ~user_id ~community_slug ~post_id
                        ~notice:None ~feedback:(Some Pages_sth.Stale_form)
                        ~status:`Forbidden
                  | `Ok fields -> (
                      let field name = List.assoc_opt name fields in
                      match (field "destination", List.length fields) with
                      (* Exactly the two application fields this form
                         emits; a third value, a duplicate, or an unknown
                         key is a generic 400 that never reaches the
                         store. *)
                      | Some destination_slug, n when n <= 2 -> (
                          let note =
                            match field "note" with Some n -> n | None -> ""
                          in
                          if String.trim destination_slug = "" then
                            respond_share request ~user_id ~community_slug
                              ~post_id ~notice:None
                              ~feedback:(Some Pages_sth.Destination_required)
                              ~status:`Bad_Request
                          else
                            (* The destination is re-resolved server-side by
                               its exact slug — the posted value identifies
                               a record and grants nothing; the store then
                               revalidates connection, eligibility, content,
                               and uniqueness under its own locks. *)
                            let%lwt resolved =
                              Dream.sql request (fun db ->
                                  Share.resolve_destination db
                                    ~slug:destination_slug)
                            in
                            match resolved with
                            | Error _ -> server_error_page request
                            | Ok None ->
                                respond_share request ~user_id ~community_slug
                                  ~post_id ~notice:None
                                  ~feedback:
                                    (Some Pages_sth.Destination_unavailable)
                                  ~status:`Conflict
                            | Ok (Some destination_community_id) ->
                                let%lwt result =
                                  Dream.sql request (fun db ->
                                      Store.request db ~actor_user_id:user_id
                                        ~post_id ~destination_community_id
                                        ~request_note:(Some note))
                                in
                                share_request_result request ~user_id
                                  ~community_slug ~post_id ~view result)
                      | _ -> bad_request_page request)))))

(* --- The management surface --- *)

let with_authorized_management request ~user_id ~community_slug k =
  let%lwt loaded =
    Dream.sql request (fun db ->
        Manage.load_for_manager db ~user_id
          ~session_global_admin:(session_global_admin request)
          ~community_slug)
  in
  match loaded with
  | Ok None | Error Manage.Invalid_community_slug -> not_found_page request
  | Error (Manage.Invalid_user_id | Manage.Invalid_placement_id) ->
      (* Defensive: the session gate already validated the id. *)
      server_error_page request
  | Error (Manage.Inconsistent_data | Manage.Storage_error) ->
      server_error_page request
  | Ok (Some view) -> k view

let respond_management request ~user_id ~community_slug ~notice ~feedback
    ~status =
  with_authorized_management request ~user_id ~community_slug (fun view ->
      let%lwt shell =
        load_launch_shell request ~user_id
          ~canonical_slug:(Manage.view_community_slug view)
          ~can_manage:true
      in
      Dream.respond ~status ~headers:page_headers
        (Pages_sth.management_page ?user:(session_user request) ~request
           ?shell
           ~state:(management_state_of_view view)
           ~notice ~feedback ()))

(* GET /c/:slug/settings/shared-threads *)
let make_management_page_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          (* Informational: no locks, no state change; the only carried
             state is the closed ?done= notice a completed mutation left. *)
          respond_management request ~user_id ~community_slug
            ~notice:(management_notice request) ~feedback:None ~status:`OK))

(* --- Shared mutation entry for the manager-gated mutations ---

   Past the session gate and the route community: the authorized view
   (which is the authorization decision), then Dream's form API — which
   enforces the URL-encoded content type and verifies its own CSRF field,
   stripping it from the returned fields. A CSRF failure refuses with 403
   and never opens the store, answered with the authorized management page
   carrying a fresh token rather than a dead end. *)
let with_management_mutation request k =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_authorized_management request ~user_id ~community_slug
            (fun view ->
              match%lwt Dream.form request with
              | `Wrong_content_type -> bad_request_page request
              | `Expired _ | `Wrong_session _ | `Invalid_token _
              | `Missing_token _ | `Many_tokens _ ->
                  respond_management request ~user_id ~community_slug
                    ~notice:None ~feedback:(Some Pages_sth.Stale_form)
                    ~status:`Forbidden
              | `Ok fields -> k ~user_id ~community_slug ~view ~fields)))

(* --- Shared subject binding for the id-addressed mutations ---

   The placement is re-read server-side from its own row and checked
   against the route community. The route id identifies the record; the
   route community decides authority, and the two must agree before the
   store is opened. Every mismatch is the same generic 404, and a stale
   status is deliberately not judged here — the store's guarded mutation
   answers it. *)
let with_bound_placement request k =
  with_placement_id request (fun ~placement_id ->
      let%lwt loaded =
        Dream.sql request (fun db ->
            Manage.load_placement_subjects db ~placement_id)
      in
      match loaded with
      | Error Manage.Invalid_placement_id -> not_found_page request
      | Error
          ( Manage.Invalid_user_id | Manage.Invalid_community_slug
          | Manage.Inconsistent_data | Manage.Storage_error ) ->
          server_error_page request
      | Ok None -> not_found_page request
      | Ok (Some subjects) -> k ~placement_id ~subjects)

(* The share-page forms mark themselves with one closed context field so a
   completed action can return to the page it came from; the marker names a
   surface this handler rebuilds itself and grants nothing. Anything else
   in the field set is refused. *)
let share_context_of_fields = function
  | [] -> Some false
  | [ ("context", "share") ] -> Some true
  | _ -> None

(* --- POST /c/:slug/settings/shared-threads/:placement_id/{accept,reject} --- *)

let review_result request ~user_id ~community_slug ~outcome = function
  | Ok _ -> Lwt.return (management_redirect ~community_slug ~outcome)
  | Error Store.Review_unavailable ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Review_unavailable) ~status:`Conflict
  | Error Store.Destination_ineligible ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Source_ineligible) ~status:`Conflict
  | Error
      ( Store.Community_unavailable | Store.No_accepted_connection
      | Store.Origin_ineligible ) ->
      (* The origin side's exact state never crosses to this page. *)
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Origin_unavailable) ~status:`Conflict
  | Error (Store.Post_unavailable | Store.Post_tombstoned) ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Thread_unavailable) ~status:`Conflict
  | Error Store.Invalid_destination_section ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Section_invalid) ~status:`Conflict
  | Error
      ( Store.Invalid_user_id | Store.Invalid_placement_id
      | Store.Invalid_post_id | Store.Invalid_community_id
      | Store.Invalid_request_note | Store.Same_community
      | Store.Active_placement_exists | Store.Withdrawal_unavailable
      | Store.Removal_unavailable | Store.Inconsistent_data
      | Store.Storage_error ) ->
      server_error_page request

let make_review_handler ~accept request =
  with_management_mutation request
    (fun ~user_id ~community_slug ~view ~fields ->
      with_bound_placement request (fun ~placement_id ~subjects ->
          let community_id = Manage.view_community_id view in
          (* Accept and reject belong to the destination alone: the origin
             side reviewing its own request is refused here, before the
             store, and again by the store's guarded mutation. *)
          if Manage.subjects_destination_community_id subjects <> community_id
          then not_found_page request
          else
            let decision =
              if not accept then Some Store.Reject
              else if Manage.view_sections_enabled view then
                (* A sectioned destination requires the accept form's one
                   section choice; the store revalidates and locks the
                   section itself, so a foreign, deleted, or absent choice
                   collapses into one refusal there. *)
                match fields with
                | [ ("section", raw) ] -> (
                    match int_of_string_opt (String.trim raw) with
                    | Some section_id when section_id > 0 ->
                        Some (Store.Accept (Some section_id))
                    | _ -> None)
                | _ -> None
              else
                (* A flat destination accepts with no section and its form
                   carries no field. *)
                match fields with
                | [] -> Some (Store.Accept None)
                | _ -> None
            in
            match (decision, accept, fields) with
            | None, true, _ ->
                respond_management request ~user_id ~community_slug
                  ~notice:None ~feedback:(Some Pages_sth.Section_invalid)
                  ~status:`Bad_Request
            | None, false, _ -> bad_request_page request
            | Some _, false, _ :: _ ->
                (* The reject form carries no application field at all. *)
                bad_request_page request
            | Some decision, _, _ ->
                let outcome = if accept then "accepted" else "rejected" in
                let%lwt result =
                  Dream.sql request (fun db ->
                      Store.review db ~reviewer_user_id:user_id ~placement_id
                        ~destination_community_id:community_id ~decision)
                in
                review_result request ~user_id ~community_slug ~outcome result))

let make_accept_handler request = make_review_handler ~accept:true request
let make_reject_handler request = make_review_handler ~accept:false request

(* --- POST /c/:slug/settings/shared-threads/:placement_id/withdraw ---

   Withdrawal is the one mutation whose policy reaches past the management
   gate: the original requester may withdraw without any moderator role, so
   the gate is its own SQL — requester, origin top_mod, or durable admin,
   bound to the route community as the placement's origin — and the refusal
   surface degrades in the same order the viewer's own surfaces do:
   management page for a manager, Share page for a sharer, and a generic
   conflict page for a requester who since lost both. *)

let withdraw_refused request ~user_id ~community_slug ~grant ~feedback ~status
    =
  let%lwt manager_view =
    Dream.sql request (fun db ->
        Manage.load_for_manager db ~user_id
          ~session_global_admin:(session_global_admin request)
          ~community_slug)
  in
  match manager_view with
  | Ok (Some _) ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some feedback) ~status
  | Ok None | Error _ -> (
      let post_id = Manage.grant_post_id grant in
      let%lwt share_view =
        Dream.sql request (fun db ->
            Share.load_share_view db ~user_id
              ~session_global_admin:(session_global_admin request)
              ~post_id)
      in
      match share_view with
      | Ok (Some view)
        when String.equal
               (Share.view_origin_community_slug view)
               community_slug ->
          respond_share request ~user_id ~community_slug ~post_id ~notice:None
            ~feedback:(Some feedback) ~status
      | Ok (Some _) | Ok None | Error _ ->
          (* A requester with no remaining surface still gets a truthful,
             payload-free answer. *)
          Dream.respond ~status ~headers:page_headers
            (Pages.msg_page ?user:(session_user request) ~title:"Unavailable"
               ~message:
                 "That sharing request is no longer pending. Nothing was \
                  changed."
               ~alert_type:"error"
               ~return_url:("/c/" ^ community_slug)
               request))

let make_withdrawal_handler request =
  with_session request (fun ~user_id ->
      with_community request (fun ~community_slug ->
          with_placement_id request (fun ~placement_id ->
              let%lwt granted =
                Dream.sql request (fun db ->
                    Manage.authorize_withdrawal db ~user_id
                      ~session_global_admin:(session_global_admin request)
                      ~community_slug ~placement_id)
              in
              match granted with
              | Ok None -> not_found_page request
              | Error
                  ( Manage.Invalid_community_slug
                  | Manage.Invalid_placement_id ) ->
                  not_found_page request
              | Error Manage.Invalid_user_id ->
                  server_error_page request
              | Error (Manage.Inconsistent_data | Manage.Storage_error) ->
                  server_error_page request
              | Ok (Some grant) -> (
                  match%lwt Dream.form request with
                  | `Wrong_content_type -> bad_request_page request
                  | `Expired _ | `Wrong_session _ | `Invalid_token _
                  | `Missing_token _ | `Many_tokens _ ->
                      withdraw_refused request ~user_id ~community_slug ~grant
                        ~feedback:Pages_sth.Stale_form ~status:`Forbidden
                  | `Ok fields -> (
                      match share_context_of_fields fields with
                      | None -> bad_request_page request
                      | Some from_share -> (
                          let%lwt result =
                            Dream.sql request (fun db ->
                                Store.withdraw db ~actor_user_id:user_id
                                  ~placement_id
                                  ~origin_community_id:
                                    (Manage.grant_community_id grant))
                          in
                          match result with
                          | Ok _ ->
                              Lwt.return
                                (if from_share then
                                   share_redirect
                                     ~origin_slug:
                                       (Manage.grant_community_slug grant)
                                     ~post_id:(Manage.grant_post_id grant)
                                     ~outcome:"withdrawn"
                                 else
                                   management_redirect ~community_slug
                                     ~outcome:"withdrawn")
                          | Error Store.Withdrawal_unavailable ->
                              withdraw_refused request ~user_id
                                ~community_slug ~grant
                                ~feedback:Pages_sth.Withdrawal_unavailable
                                ~status:`Conflict
                          | Error _ -> server_error_page request))))))

(* --- POST /c/:slug/settings/shared-threads/:placement_id/remove --- *)

let removal_result request ~user_id ~community_slug ~from_share ~subjects =
  function
  | Ok _ ->
      Lwt.return
        (if
           from_share
           (* The Share page lives on the origin side only; a destination
              manager's removal always returns to their own management
              page. *)
         then
           share_redirect
             ~origin_slug:community_slug
             ~post_id:(Manage.subjects_post_id subjects)
             ~outcome:"removed"
         else management_redirect ~community_slug ~outcome:"removed")
  | Error Store.Removal_unavailable ->
      respond_management request ~user_id ~community_slug ~notice:None
        ~feedback:(Some Pages_sth.Removal_unavailable) ~status:`Conflict
  | Error _ -> server_error_page request

let make_removal_handler request =
  with_management_mutation request
    (fun ~user_id ~community_slug ~view ~fields ->
      with_bound_placement request (fun ~placement_id ~subjects ->
          let community_id = Manage.view_community_id view in
          if
            Manage.subjects_origin_community_id subjects <> community_id
            && Manage.subjects_destination_community_id subjects
               <> community_id
          then
            (* The route community is not part of this placement: the id
               names someone else's record. *)
            not_found_page request
          else
            match share_context_of_fields fields with
            | None -> bad_request_page request
            | Some from_share ->
                (* Either side may remove, so the acting community is the
                   route community — already proved to be one of the pair —
                   and it is what the store's guarded mutation verifies. The
                   Share-page return applies only when that side is the
                   origin. *)
                let from_share =
                  from_share
                  && Manage.subjects_origin_community_id subjects
                     = community_id
                in
                let%lwt result =
                  Dream.sql request (fun db ->
                      Store.remove db ~actor_user_id:user_id ~placement_id
                        ~acting_community_id:community_id)
                in
                removal_result request ~user_id ~community_slug ~from_share
                  ~subjects result))
