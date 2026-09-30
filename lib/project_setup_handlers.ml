(* HTTP layer for project setup over verified GitHub drafts. Both
   handlers are factories over the closed onboarding mode (and, for the POST,
   a config loader) so tests can inject fixed values without touching the
   process environment, and disabled or rejected requests never trigger
   configuration reads or SQL. Nothing here logs a query or form value. *)

module Read = Project_onboarding_draft_read_model
module Sel = Project_onboarding_draft_selection_store
module Form = Project_setup_repository_form
module Pages_ps = Project_setup_pages

(* --- Session reads (identical rules to the GitHub onboarding handlers) --- *)

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

(* Every rendered page in this feature reflects session identity, draft
   state, and one-time feedback: never cacheable, and the query never leaks
   onward as a cross-origin Referer. The
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
    (Site_pages.msg_page ?user:(session_user request) ~title:"Not Allowed"
       ~message:"This request is not allowed." ~alert_type:"error"
       ~return_url:"/projects/new" request)

let bad_request_page request =
  Dream.respond ~status:`Bad_Request ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Form Error"
       ~message:"There was a problem with your submission. Please try again."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* One generic 500 for every read-model or store failure past the gates —
   no Caqti/PostgreSQL detail, error constructor, or fixture value ever
   reaches the page. *)
let server_error_page request =
  Dream.respond ~status:`Internal_Server_Error ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request) ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* Configuration-loading failure on the POST: a deployment problem, answered
   like the GitHub start handler's — generic, non-cacheable, and never
   disguised as a user form error. *)
let unavailable_page request =
  Dream.respond ~status:`Service_Unavailable ~headers:page_headers
    (Site_pages.msg_page ?user:(session_user request)
       ~title:"Temporarily Unavailable"
       ~message:
         "Project setup is temporarily unavailable. Please try again later."
       ~alert_type:"error" ~return_url:"/projects/new" request)

(* --- Raw query parsing (GET /projects/new) --- *)

(* Every occurrence of one raw query key in the original request target, in
   order: [Some value] for [key=value] (the value byte-for-byte after the
   first '='), [None] for a bare [key]. Dream.query silently tolerates
   duplicate keys, so the target is split by hand: only the substring after
   the first '?', stopped at a raw '#', components on raw '&'. Keys are
   case-sensitive; unrelated parameters are ignored. Values are never
   percent-decoded or repaired — decoding would create aliases of the strict
   grammars below. *)
let raw_query_occurrences ~key target =
  match String.index_opt target '?' with
  | None -> []
  | Some q ->
      let query = String.sub target (q + 1) (String.length target - q - 1) in
      let query =
        match String.index_opt query '#' with
        | None -> query
        | Some h -> String.sub query 0 h
      in
      List.filter_map
        (fun component ->
          match String.index_opt component '=' with
          | None -> if String.equal component key then Some None else None
          | Some eq ->
              if String.equal (String.sub component 0 eq) key then
                Some
                  (Some
                     (String.sub component (eq + 1)
                        (String.length component - eq - 1)))
              else None)
        (String.split_on_char '&' query)

(* Strict positive decimal int64: ASCII digits only, must fit int64, must be
   positive. Leading zeroes are tolerated — they cannot alias a different id
   — but signs, whitespace, decimal points, hex, and overflow are rejected. *)
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

(* The three shapes of the explicit draft selector. An invalid explicit
   selector must never become a database identifier: it collapses to the
   normal chooser/empty flow with generic unavailable feedback. *)
type draft_selector =
  | No_draft_requested
  | Invalid_draft_selector
  | Requested_draft of int64

let draft_selector_of_target target =
  match raw_query_occurrences ~key:"draft" target with
  | [] -> No_draft_requested
  | [ Some value ] -> (
      match positive_int64_of_digits value with
      | Some id -> Requested_draft id
      | None -> Invalid_draft_selector)
  (* Bare keys, blanks (caught above as non-digits), and duplicates. *)
  | _ -> Invalid_draft_selector

(* Cosmetic one-time feedback: exactly one recognized case-sensitive value,
   duplicate-aware; anything else renders none. The raw value never reaches
   the page or a log — only the closed variant crosses, and feedback never
   changes authorization or which rows are read. *)
let feedback_of_target target =
  match raw_query_occurrences ~key:"selection" target with
  | [ Some "saved" ] -> Some Pages_ps.Selection_saved
  | [ Some "stale" ] -> Some Pages_ps.Selection_stale
  | [ Some "invalid" ] -> Some Pages_ps.Selection_invalid
  | [ Some "unavailable" ] -> Some Pages_ps.Draft_unavailable
  | [ Some "required" ] -> Some Pages_ps.Repository_selection_required
  | _ -> None

(* The identity step is entered by exactly one case-sensitive [step=details].
   Every other shape — absent, blank, bare, duplicated, unknown, re-cased, or
   a percent-encoded lookalike — is the normal repository step, with no
   diagnostic: the raw value is never decoded, repaired, or reflected. *)
let details_step_of_target target =
  match raw_query_occurrences ~key:"step" target with
  | [ Some "details" ] -> true
  | _ -> false

(* --- Read model → page view models (mechanical, public accessors only;
   installation ids, account ids, GitHub repository ids, provenance, and
   timestamps deliberately never cross) --- *)

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

let repository_option_of_read repository : Pages_ps.repository_option =
  {
    Pages_ps.snapshot_id = Read.snapshot_id repository;
    full_name = Read.full_name repository;
    html_url = Read.html_url repository;
    description = Read.description repository;
    default_branch = Read.default_branch repository;
    is_archived = Read.is_archived repository;
    is_selected = Read.is_selected repository;
  }

let configuration_of_view view : Pages_ps.configuration =
  {
    Pages_ps.draft = draft_option_of_summary (Read.summary view);
    repositories =
      List.map repository_option_of_read (Read.repositories view);
  }

let identity_repository_of_read repository : Pages_ps.identity_repository =
  {
    Pages_ps.snapshot_id = Read.snapshot_id repository;
    full_name = Read.full_name repository;
    is_archived = Read.is_archived repository;
  }

(* The permanent identity is the user's to state: nothing is prefilled from
   the account login, repository names, or GitHub metadata, and no primary is
   inferred even from a single selected repository. *)
let initial_identity_values : Pages_ps.identity_values =
  {
    Pages_ps.kind = Project_identity.Project;
    name = "";
    slug = "";
    description = "";
    website_url = "";
    primary_snapshot_id = None;
  }

(* Only currently selected rows cross into the identity step, in snapshot
   order — the same public-accessor discipline as the repository step, minus
   URLs, descriptions, and branches the step does not show. *)
let identity_configuration_of_view view : Pages_ps.identity_configuration =
  {
    Pages_ps.draft = draft_option_of_summary (Read.summary view);
    selected_repositories =
      List.filter_map
        (fun repository ->
          if Read.is_selected repository then
            Some (identity_repository_of_read repository)
          else None)
        (Read.repositories view);
    values = initial_identity_values;
  }

(* --- GET /projects/new --- *)

(* The session username is layout display context only — never an
   authorization source. The request is passed through so the configuration
   form carries Dream's framework CSRF field. *)
let render_page request ~state ~feedback =
  Dream.html ~headers:page_headers
    (Pages_ps.project_setup_page
       ?user:(session_user request)
       ~request ~state ~feedback ())

(* One owner-authorized view, rendered for the requested step. The read model
   is authoritative on every GET: entering the identity step with no
   currently selected row — including the legitimate race where a GitHub
   refresh replaced the snapshot between the selection POST and the
   redirected GET — falls back to the repository step with its dedicated
   feedback, which overrides any cosmetic query feedback. *)
let render_view request ~details ~feedback view =
  if not details then
    render_page request
      ~state:(Pages_ps.Configure_repositories (configuration_of_view view))
      ~feedback
  else if List.exists Read.is_selected (Read.repositories view) then
    render_page request
      ~state:
        (Pages_ps.Configure_identity (identity_configuration_of_view view))
      ~feedback
  else
    render_page request
      ~state:(Pages_ps.Configure_repositories (configuration_of_view view))
      ~feedback:(Some Pages_ps.Repository_selection_required)

let list_available request ~user_id =
  Dream.sql request (fun db -> Read.list_available db ~user_id)

let load_available request ~user_id ~draft_id =
  Dream.sql request (fun db -> Read.load_available db ~user_id ~draft_id)

(* The chooser/empty flow shared by "no explicit draft" and every
   unavailable-selector fallback. No draft is ever chosen automatically when
   several exist; a single listed draft is re-loaded — and thereby
   re-authorized — through load_available rather than rendered from
   summary-only data. If it disappears between list and load, the list
   decision is repeated exactly once and a still-singular list falls back to
   the chooser: bounded, never a loop. *)
let render_from_list request ~user_id ~details ~feedback =
  let%lwt listed = list_available request ~user_id in
  match listed with
  | Error _ -> server_error_page request
  | Ok [] -> render_page request ~state:Pages_ps.No_available_drafts ~feedback
  | Ok [ only ] -> (
      let%lwt loaded =
        load_available request ~user_id ~draft_id:(Read.draft_id only)
      in
      match loaded with
      | Error _ -> server_error_page request
      | Ok (Some view) -> render_view request ~details ~feedback view
      | Ok None -> (
          let%lwt relisted = list_available request ~user_id in
          match relisted with
          | Error _ -> server_error_page request
          | Ok [] ->
              render_page request ~state:Pages_ps.No_available_drafts ~feedback
          | Ok options ->
              render_page request
                ~state:
                  (Pages_ps.Choose_draft
                     (List.map draft_option_of_summary options))
                ~feedback))
  | Ok options ->
      render_page request
        ~state:(Pages_ps.Choose_draft (List.map draft_option_of_summary options))
        ~feedback

let make_new_project_handler ~mode request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no query read, no SQL — one clean redirect. *)
      Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Lwt.return (login_redirect ())
      | Some user_id ->
          let is_admin =
            session_field_opt request "is_admin" = Some "true"
          in
          if not (Project_onboarding.onboarding_available mode ~is_admin) then
            (* Rollout-limited: same clean target as Off, so the redirect
               reveals no feature-flag names or rollout detail. *)
            Lwt.return (bring_redirect ())
          else
            let target = Dream.target request in
            let feedback = feedback_of_target target in
            let details = details_step_of_target target in
            (match draft_selector_of_target target with
            | Requested_draft draft_id -> (
                let%lwt loaded = load_available request ~user_id ~draft_id in
                match loaded with
                | Error _ -> server_error_page request
                | Ok (Some view) ->
                    render_view request ~details ~feedback view
                | Ok None ->
                    (* Nonexistent, foreign, expired, terminal, and revoked
                       stay indistinguishable; the generic unavailable
                       feedback overrides any supplied one, and the requested
                       step is dropped with the draft so the fallback stays
                       the established chooser/empty behavior. *)
                    render_from_list request ~user_id ~details:false
                      ~feedback:(Some Pages_ps.Draft_unavailable))
            | Invalid_draft_selector ->
                render_from_list request ~user_id ~details:false
                  ~feedback:(Some Pages_ps.Draft_unavailable)
            | No_draft_requested ->
                render_from_list request ~user_id ~details ~feedback))

(* --- POST /projects/new/repositories --- *)

(* PRG targets, built structurally — never by concatenating request data.
   Only a store-authorized draft id ever reaches a Location. *)
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

let details_redirect ~draft_id =
  clean_redirect
    (Uri.to_string
       (Uri.make ~path:"/projects/new"
          ~query:
            [ ("draft", [ Int64.to_string draft_id ]);
              ("step", [ "details" ]);
            ]
          ()))

let make_repository_selection_handler ~mode ~load_config request =
  match mode with
  | Project_onboarding.Off ->
      (* Kill switch: no configuration read, no form read, no SQL. *)
      Lwt.return (bring_redirect ())
  | Project_onboarding.Admins | Project_onboarding.Public -> (
      match authenticated_user_id request with
      | None -> Lwt.return (login_redirect ())
      | Some user_id ->
          let is_admin =
            session_field_opt request "is_admin" = Some "true"
          in
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
                          (* The malformed form's draft id is untrusted and
                             deliberately not recovered. *)
                          Lwt.return (feedback_redirect "invalid")
                      | Ok form -> (
                          let draft_id = Form.draft_id form in
                          let%lwt replaced =
                            Dream.sql request (fun db ->
                                Sel.replace db ~user_id ~draft_id
                                  ~selected_snapshot_ids:
                                    (Form.selected_snapshot_ids form)
                                  (* The primary repository is intentionally
                                     not chosen in this step. *)
                                  ~primary_snapshot_id:None)
                          in
                          match replaced with
                          | Ok () ->
                              (* The one durable-acceptance boundary of this
                                 step, shared by both continuations below: the
                                 store committed the replacement. Only the
                                 accepted set's SIZE crosses — never a
                                 snapshot id, repository name, or URL. A
                                 deliberately cleared selection is a committed
                                 transition too, and reports count 0. *)
                              Analytics.capture_if_consented request
                                ~distinct_id:
                                  (Analytics.distinct_id_of_user_id user_id)
                                (Analytics.Github_repositories_selected
                                   {
                                     user_id;
                                     repository_count =
                                       List.length
                                         (Form.selected_snapshot_ids form);
                                   });
                              if Form.selected_snapshot_ids form = [] then
                                (* The empty selection is deliberately
                                   saved, but the identity step needs at
                                   least one repository: stay on the
                                   repository step with its dedicated
                                   feedback. *)
                                Lwt.return (draft_redirect ~draft_id "required")
                              else
                                (* Straight to the project-details step; the
                                   redirected GET re-reads the authoritative
                                   selection rather than trusting that this
                                   one survived. *)
                                Lwt.return (details_redirect ~draft_id)
                          | Error Sel.Selection_stale ->
                              (* The next GET reloads the fresh snapshot. *)
                              Lwt.return (draft_redirect ~draft_id "stale")
                          | Error Sel.Invalid_selection ->
                              (* Defensive: the pure parser already enforced
                                 the structural rules. The caller was
                                 authorized for this draft, so its id may
                                 ride along. *)
                              Lwt.return (draft_redirect ~draft_id "invalid")
                          | Error Sel.Draft_unavailable ->
                              (* Possibly foreign, expired, or nonexistent:
                                 the submitted id is dropped from the URL. *)
                              Lwt.return (feedback_redirect "unavailable")
                          | Error (Sel.Invalid_user_id | Sel.Invalid_draft_id)
                            ->
                              Lwt.return (feedback_redirect "invalid")
                          | Error (Sel.Inconsistent_data | Sel.Storage_error)
                            ->
                              (* A server failure is never disguised as a
                                 user form error. *)
                              server_error_page request)))))
