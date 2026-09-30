(* Pending-signup confirmation: hashing the URL token and matching it is what creates the
   real users row (in one DB transaction). Per product decision we do NOT auto-login — the
   user is sent to /login. Replay (already-consumed) and expired tokens both fail as `Invalid;
   a username/email taken since signup fails as `Conflict. Login/session logic is untouched. *)
(* === ANALYTICS CONSENT (spec §9) === *)

(* Session read that also behaves in tests where no session middleware is
   installed: no middleware simply means no authenticated session. *)
let session_user_id_opt request =
  match Dream.session_field request "user_id" with
  | exception _ -> None
  | value -> value

(* Analytics-only person sync on consent grant. Every failure mode (bad id,
   missing pool, DB error) is swallowed and logged without email/tokens —
   the consent cookie must be set regardless. *)
let consent_grant_person_sync request =
  match session_user_id_opt request with
  | None -> Lwt.return_unit
  | Some uid_str ->
      Lwt.catch
        (fun () ->
          match int_of_string_opt uid_str with
          | None -> Lwt.return_unit
          | Some user_id -> (
              let%lwt props =
                Dream.sql request (fun db ->
                    User_store.get_user_analytics_props db user_id)
              in
              match props with
              | Ok (Some (username, _email, signup_date, is_admin)) ->
                  (* Email is deliberately excluded from the closed person
                     properties — it never reaches PostHog. *)
                  Analytics.sync_person_after_consent_grant
                    ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                    { Analytics.username; signup_date; is_admin };
                  Lwt.return_unit
              | Ok None -> Lwt.return_unit
              | Error e ->
                  Dream.log "analytics consent sync lookup failed (user %d): %s"
                    user_id e;
                  Lwt.return_unit))
        (fun exn ->
          Dream.log "analytics consent sync skipped: %s"
            (Printexc.to_string exn);
          Lwt.return_unit)

(* No Dream CSRF and no session required (a first-time landing visitor has
   neither); protection is the §9 JSON-only + Origin/Sec-Fetch-Site check in
   Analytics.validate_consent_request. All responses are controlled JSON/204 —
   never a rendered HTML error page. *)
let analytics_consent_handler request =
  let%lwt body = Dream.body request in
  match
    Analytics.validate_consent_request
      ~content_type:(Dream.header request "Content-Type")
      ~origin:(Dream.header request "Origin")
      ~sec_fetch_site:(Dream.header request "Sec-Fetch-Site")
      ~body
  with
  | Error (`Forbidden reason) ->
      Dream.json ~status:`Forbidden (Printf.sprintf {|{"error":%S}|} reason)
  | Error (`Bad_request reason) ->
      Dream.json ~status:`Bad_Request (Printf.sprintf {|{"error":%S}|} reason)
  | Ok state ->
      (* Person sync runs only on granted, only for an authenticated session,
         and can never fail the response. The just-granted value exists only
         in the outgoing Set-Cookie, so this transition uses the dedicated
         sync function, not capture_if_consented (§3.1). *)
      let%lwt () =
        match state with
        | `Granted -> consent_grant_person_sync request
        | `Denied -> Lwt.return_unit
      in
      let value = match state with `Granted -> "granted" | `Denied -> "denied" in
      let response = Dream.response ~status:`No_Content "" in
      (* Plaintext (encrypt:false) and no HttpOnly: the §9 contract requires
         document.cookie readability (the prerendered landing can determine
         consent only client-side). Secure follows the public origin scheme.
         ~prefix:None is load-bearing: without it Dream infers __Host- for a
         Secure + Path=/ cookie, breaking the exact cross-repo cookie name. *)
      Dream.set_cookie ~prefix:None ~encrypt:false
        ~max_age:Analytics.consent_cookie_max_age ~path:(Some "/")
        ~secure:(Analytics.consent_cookie_secure ())
        ~http_only:false ~same_site:(Some `Lax) response request
        Analytics.consent_cookie_name value;
      Lwt.return response

(* Same exact path, non-POST methods: controlled JSON 405, never landing or
   error HTML (deployment must route the path to Dream before any static
   fallback — spec §10.4). *)
let analytics_consent_method_not_allowed _request =
  Dream.json ~status:`Method_Not_Allowed
    ~headers:[ ("Allow", "POST") ]
    {|{"error":"method not allowed"}|}

(* Domain events: a success path inside a Dream.sql block RECORDS its
   emission; the recorded thunk runs only after the block returns and its
   pooled connection is released, so the fire-and-forget analytics HTTP never
   overlaps a checked-out DB connection. Nothing recorded ⇒ nothing emitted,
   so validation/authorization/DB failures stay silent by construction, and a
   thunk can only call the closed consent-gated Analytics entry points. *)
let with_analytics_after_sql make_response =
  let pending = ref (fun () -> ()) in
  let%lwt response = make_response (fun thunk -> pending := thunk) in
  !pending ();
  Lwt.return response

(* Centralized private-safe analytics shaping (§13). Fully private
   communities keep numeric ids, counts, and the community:<id> group key,
   but no human-readable identifier (slug, name, channel/section slug) ever
   enters an analytics payload. Every handler and the $groupidentify mapping
   below go through this ONE helper — no per-handler visibility checks. The
   visibility comes from the authoritative community record the handler
   already loaded; no analytics-only DB query exists. *)
let analytics_public_string (community : Community_types.community) value =
  if Community_types.community_is_private community.Community_types.visibility then None else Some value

(* The closed $groupidentify record from an authoritative Community_types.community row.
   The shared community record carries no created_at column, so that optional
   group property is omitted rather than approximated. Private communities:
   slug and name are None (§13), id and closed visibility remain. *)
let community_group_of (community : Community_types.community) : Analytics.community_group =
  {
    Analytics.community_id = community.id;
    community_slug = analytics_public_string community community.slug;
    community_name = analytics_public_string community community.name;
    community_visibility =
      Community_types.community_visibility_to_string community.Community_types.visibility;
    created_at = None;
  }

(* Immediate async attempt for a freshly enqueued §13 group-cleanup job —
   the exact analog of attempt_posthog_deletion_job: claim, one bounded HTTP
   scrub, mark. Every DB touch is its own short call; failures only leave
   the durable job pending. Logs carry the job id and bounded error classes
   only — never a community name or slug. *)
let attempt_posthog_group_cleanup_job request ~job_id =
  Lwt.catch
    (fun () ->
      let%lwt claimed =
        Dream.sql request (fun db -> Posthog_group_cleanup_job_store.claim db job_id)
      in
      match claimed with
      | Ok (Some group_key) ->
          let%lwt (_ : [ `Completed | `Left_pending of string ]) =
            Posthog_deletion.process_claimed_group_job
              ~mark_completed:(fun () ->
                Dream.sql request (fun db ->
                    Posthog_group_cleanup_job_store.mark_completed db job_id))
              ~mark_failed:(fun err ->
                Dream.sql request (fun db ->
                    Posthog_group_cleanup_job_store.mark_failed db job_id err))
              ~group_key
          in
          Lwt.return_unit
      | Ok None | Error _ -> Lwt.return_unit)
    (fun exn ->
      Dream.log "posthog group cleanup immediate attempt error: %s"
        (Printexc.to_string exn);
      Lwt.return_unit)
