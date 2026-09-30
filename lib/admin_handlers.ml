(* === ADMIN ===

   PostHog is the authoritative KPI/analytics product; the old in-app KPI
   dashboard (GET /earde-hq-dashboard) was removed with its renderer and its
   two exclusive aggregate queries. Nothing replaced it and no route redirects
   to PostHog — /admin stays the operational admin surface. *)

(* Authority here is CURRENT durable users.is_admin, resolved before the form
   is even parsed: the session claim only decides whether the lookup is worth
   making. A demoted operator's live session lands on the same "not an Admin"
   response an ordinary user gets, and a lookup that cannot be answered is a
   generic failure with no ban written. *)
let ban_user_handler request =
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ ->
      Dream.respond ~status:`Internal_Server_Error
        (Site_pages.msg_page ~title:"Error" ~message:Handler_support.generic_db_error ~alert_type:"error" ~return_url:"/" request)
  | Current_admin -> (
      (* The ban form carries only Dream's CSRF field; parsing the body is
         what actually validates the session-bound token, and it must happen
         before any mutation. Path ids never grant authority on their own. *)
      match%lwt Dream.form request with
      | `Ok _ ->
      let user_id_to_ban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_ban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Admin_store.ban_user db user_id_to_ban with
        | Ok () ->
            (* Notify banned user — best-effort; no post to link to. *)
            let%lwt _ = Notification_store.create_notif db user_id_to_ban None "mod_action" "You have been globally banned by an administrator." in
            (* Redirect back to the profile page rather than "/" so the admin
               immediately sees the updated 🚫 badge and the Unban button. *)
            let target = Handler_support.safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
            Dream.redirect request target
        | Error err -> Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/admin" request)
      )
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request))
  | Current_non_admin -> Dream.html (Site_pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

(* Same current-authority contract as ban_user_handler. *)
let unban_user_global_handler request =
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ ->
      Dream.respond ~status:`Internal_Server_Error
        (Site_pages.msg_page ~title:"Error" ~message:Handler_support.generic_db_error ~alert_type:"error" ~return_url:"/" request)
  | Current_admin -> (
      (* Same contract as ban_user_handler: the unban form has no application
         fields, but Dream.form must still run — it is the CSRF validation. *)
      match%lwt Dream.form request with
      | `Ok _ ->
      let user_id_to_unban = try int_of_string (Dream.param request "id") with _ -> 0 in
      if user_id_to_unban = 0 then Dream.respond ~status:`Bad_Request "Invalid user ID." else
      Dream.sql request (fun db ->
        match%lwt Admin_store.unban_user_global db user_id_to_unban with
        | Ok () ->
            let target = Handler_support.safe_local_redirect ~default:"/admin" request (match Dream.header request "Referer" with Some r -> r | None -> "/admin") in
            Dream.redirect request target
        | Error err -> Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/admin" request)
      )
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/admin" request))
  | Current_non_admin -> Dream.html (Site_pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

(* The dashboard is admin-only PII (recent users, pending signups, the global
   ban list), so it is gated on CURRENT durable authority; an unanswerable
   lookup renders none of it. *)
let admin_dashboard_handler request =
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ ->
      Dream.respond ~status:`Internal_Server_Error
        (Site_pages.msg_page ~title:"Error" ~message:Handler_support.generic_db_error ~alert_type:"error" ~return_url:"/" request)
  | Current_admin ->
      let user = Dream.session_field request "username" in
      (* Safe config status only — the Turnstile site-key payload in [Configured] is
         discarded here so no secret/key reaches the page; Email.is_configured returns a
         bool, never the API key. *)
      let signups_enabled = Auth_handlers.signups_enabled () in
      let turnstile = match Turnstile.status () with
        | Turnstile.Configured _ -> `Configured
        | Turnstile.Disabled     -> `Disabled
        | Turnstile.Misconfigured -> `Misconfigured
      in
      let brevo_configured = Email.is_configured () in
      Dream.sql request (fun db ->
        let%lwt banned_res  = Admin_store.get_globally_banned_users db in
        let%lwt recent_res  = Admin_store.list_recent_users db ~limit:50 in
        let%lwt pending_res = Admin_store.list_recent_pending db ~limit:50 in
        (* Joined communities feed the shared launch rail only; loaded here —
           after the admin gate — so denied requests never touch membership
           data, and a failure degrades to an empty rail rather than blocking
           the dashboard. *)
        let%lwt rail_res =
          match Option.bind (Dream.session_field request "user_id") int_of_string_opt with
          | Some uid -> Membership_store.get_user_communities db uid
          | None -> Lwt.return (Ok [])
        in
        let rail_communities = match rail_res with Ok cs -> cs | Error _ -> [] in
        match banned_res, recent_res, pending_res with
        | Ok banned_users, Ok recent_users, Ok pending ->
            Dream.html (Admin_pages.admin_dashboard_page ?user ~rail_communities ~signups_enabled ~turnstile
              ~brevo_configured ~recent_users ~pending ~banned_users request)
        | (Error e, _, _) | (_, Error e, _) | (_, _, Error e) ->
            Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )
  | Current_non_admin -> Dream.respond ~status:`Forbidden (Site_pages.msg_page ~title:"Access Denied" ~message:"You are not an Admin." ~alert_type:"error" ~return_url:"/" request)

(* Admin-only: GC stats expose heap pressure without a profiler attachment.
   Cheaper than pprof; useful for spotting minor-GC spikes on staging.
   It also echoes the caller's own session dictionary, so it is gated on
   CURRENT durable authority like every other admin-only surface. *)
let debug_state_handler request =
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ ->
      Dream.respond ~status:`Internal_Server_Error
        ~headers:[("Content-Type", "application/json")]
        {|{"error":"unavailable"}|}
  | Current_admin ->
      let gc = Gc.stat () in
      let sess_field k =
        match Dream.session_field request k with Some s -> `String s | None -> `Null
      in
      let gc_json = `Assoc [
        ("minor_words",       `Float gc.Gc.minor_words);
        ("promoted_words",    `Float gc.Gc.promoted_words);
        ("major_words",       `Float gc.Gc.major_words);
        ("minor_collections", `Int   gc.Gc.minor_collections);
        ("major_collections", `Int   gc.Gc.major_collections);
        ("compactions",       `Int   gc.Gc.compactions);
        ("heap_words",        `Int   gc.Gc.heap_words);
        ("live_words",        `Int   gc.Gc.live_words);
        ("free_words",        `Int   gc.Gc.free_words);
      ] in
      let session_json = `Assoc [
        ("user_id",  sess_field "user_id");
        ("username", sess_field "username");
        ("is_admin", sess_field "is_admin");
      ] in
      let body = Yojson.Safe.pretty_to_string (`Assoc [
        ("gc",      gc_json);
        ("session", session_json);
      ]) in
      Dream.respond ~headers:[("Content-Type", "application/json")] body
  | Current_non_admin ->
      Dream.respond ~status:`Forbidden
        ~headers:[("Content-Type", "application/json")]
        {|{"error":"forbidden"}|}
