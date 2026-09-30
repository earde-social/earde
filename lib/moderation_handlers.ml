let modlog_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> ( try int_of_string id with _ -> 0)
    | None -> 0
  in
  Dream.sql request (fun db ->
      (* CURRENT durable admin authority for the private-community read gate
       (and, below, for the presentation-only settings back-link). *)
      let%lwt is_admin =
        Admin_authority.current_admin_read_override db request
      in
      match%lwt Community_store.get_community_by_slug db slug with
      | Ok (Some community) -> (
          let%lwt authorized =
            Community_read_gate.can_view_community db ~user_id
              ~admin_override:is_admin community
          in
          if not authorized then
            Community_read_gate.community_not_found ?user request
          else
            (* Settings access mirrors community_settings_handler's gate (admin || moderator); it only
           picks the back-link target, so a failed lookup safely degrades to "Back to community". *)
            let%lwt can_access_settings =
              if is_admin then Lwt.return true
              else
                match%lwt
                  Moderator_store.is_moderator db user_id community.id
                with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false
            in
            (* Launch-chrome data, loaded only after the private-community
           authorization decision above: sections/channels feed the shared community
           sidebar, the viewer's joined communities the global rail. Each degrades to
           an empty list on error rather than blocking the log; anonymous viewers
           never touch membership data. *)
            let%lwt sections =
              if community.sections_enabled then
                match%lwt
                  Section_store.get_sections_by_community db community.id
                with
                | Ok secs -> Lwt.return secs
                | Error _ -> Lwt.return []
              else Lwt.return []
            in
            let%lwt channels =
              match%lwt
                Channel_store.get_channels_by_community db community.id
              with
              | Ok cs -> Lwt.return cs
              | Error _ -> Lwt.return []
            in
            let%lwt rail_communities =
              if user_id > 0 then
                match%lwt Membership_store.get_user_communities db user_id with
                | Ok cs -> Lwt.return cs
                | Error _ -> Lwt.return []
              else Lwt.return []
            in
            match%lwt Mod_log_store.get_modlog db community.id with
            | Ok actions ->
                Dream.html
                  (Moderation_pages.mod_log_page ?user
                     ~noindex:(Community_read_gate.community_noindex community)
                     ~rail_communities ~can_access_settings ~channels ~sections
                     ~community actions request)
            | Error err ->
                Dream.respond ~status:`Internal_Server_Error
                  (Site_pages.msg_page ?user ~title:"Error"
                     ~message:(Handler_support.db_error_message err)
                     ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
      | Ok None ->
          Dream.respond ~status:`Not_Found
            (Site_pages.msg_page ?user ~title:"Not Found"
               ~message:"This community does not exist." ~alert_type:"error"
               ~return_url:"/" request)
      | Error err ->
          Dream.respond ~status:`Internal_Server_Error
            (Site_pages.msg_page ?user ~title:"Error"
               ~message:(Handler_support.db_error_message err)
               ~alert_type:"error" ~return_url:"/" request))

(* There are deliberately no add_mod_handler / remove_mod_handler. The
   /add-mod and /remove-mod routes they served were unreferenced legacy
   endpoints — no form, link, script or test emitted them — with strictly
   weaker authorization than the surface that replaced them: they admitted
   ANY moderator of the community, so an ordinary mod could appoint further
   moderators and unseat the Top Mod. Moderator management now lives only on
   /c/:slug/manage-mods/{add,promote,remove}, which requires top_mod (or a
   durable global admin) and refuses to remove a top_mod target. *)

let ban_community_user_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id =
            try
              int_of_string
                (List.assoc_opt "community_id" form_data
                |> Option.value ~default:"")
            with _ -> 0
          in
          let target_username =
            String.trim
              (List.assoc_opt "target_username" form_data
              |> Option.value ~default:"")
          in
          let reason =
            String.trim
              (List.assoc_opt "reason" form_data |> Option.value ~default:"")
          in
          if community_id = 0 || target_username = "" then
            Dream.respond ~status:`Bad_Request
              (Site_pages.msg_page
                 ?user:(Dream.session_field request "username")
                 ~title:"Form Error" ~message:"Invalid form data."
                 ~alert_type:"error" ~return_url:"/" request)
          else
            Dream.sql request (fun db ->
                (* TOCTOU guard: re-verify authorization at mutation time, not just at render.
               Separate is_community_mod from is_admin so we can log admin overrides distinctly.
               is_admin is CURRENT durable authority, so a stale demoted-admin
               session is evaluated exactly as a plain user — including for the
               admin-immunity and mod-log-attribution decisions below. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page
                         ?user:(Dream.session_field request "username")
                         ~title:"Error"
                         ~message:(Handler_support.db_error_message e)
                         ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin -> (
                    let%lwt is_community_mod =
                      match%lwt
                        Moderator_store.is_moderator db user_id community_id
                      with
                      | Ok b -> Lwt.return b
                      | _ -> Lwt.return false
                    in
                    let is_authorized = is_admin || is_community_mod in
                    if not is_authorized then
                      Dream.respond ~status:`Forbidden
                        (Site_pages.msg_page
                           ?user:(Dream.session_field request "username")
                           ~title:"Access Denied"
                           ~message:"You are not a moderator of this community."
                           ~alert_type:"error" ~return_url:"/" request)
                    else
                      match%lwt
                        User_store.get_user_by_username db target_username
                      with
                      | Ok (Some target_user) -> (
                          (* Admin immunity: local mods cannot ban global admins.
                     The lookup decides ONLY the local moderator's case — a
                     current global admin overrides the immunity either way —
                     so an admin actor never makes it, and the immunity keeps
                     three distinct outcomes rather than a boolean: immune,
                     not immune, and unanswerable. Unanswerable is an internal
                     failure, not evidence that the target is bannable, so it
                     ends the request before the ban, the mod-log row and the
                     notification below. *)
                          let%lwt target_immunity =
                            if is_admin then Lwt.return (Ok false)
                            else User_store.is_user_admin db target_user.id
                          in
                          match target_immunity with
                          | Error e ->
                              Dream.respond ~status:`Internal_Server_Error
                                (Site_pages.msg_page
                                   ?user:
                                     (Dream.session_field request "username")
                                   ~title:"Error"
                                   ~message:(Handler_support.db_error_message e)
                                   ~alert_type:"error" ~return_url:"/" request)
                          | Ok true ->
                              Dream.html
                                (Site_pages.msg_page
                                   ?user:
                                     (Dream.session_field request "username")
                                   ~title:"Action Denied"
                                   ~message:
                                     "You cannot ban a Global Administrator."
                                   ~alert_type:"error" ~return_url:"/" request)
                          | Ok false -> begin
                              (* Fetch community slug before the ban so we can redirect to /c/slug after. *)
                              let%lwt community_res =
                                Community_store.get_community_by_id db
                                  community_id
                              in
                              let%lwt _ =
                                Community_ban_store.ban_user db target_user.id
                                  community_id
                              in
                              (* Admin acting without mod role logged distinctly to prevent spoofing the mod log. *)
                              let is_admin_override =
                                is_admin && not is_community_mod
                              in
                              let action_type =
                                if is_admin_override then "admin_ban_user"
                                else "ban_user"
                              in
                              let logged_reason =
                                if is_admin_override then
                                  "Admin Intervention: " ^ reason
                                else reason
                              in
                              let%lwt _ =
                                Mod_log_store.log_action db community_id user_id
                                  action_type (Some target_user.id)
                                  logged_reason
                              in
                              (* Notify banned user — no post_id since a ban is not tied to a single post. *)
                              let ban_msg =
                                "You have been banned from a community. \
                                 Reason: " ^ reason
                              in
                              let%lwt _ =
                                Notification_store.create_notif db
                                  target_user.id None "mod_action" ban_msg
                              in
                              let target =
                                match community_res with
                                | Ok (Some c) ->
                                    "/c/" ^ c.slug ^ "/settings?panel=bans"
                                | _ -> "/"
                              in
                              Dream.redirect request target
                            end)
                      | Ok None ->
                          Dream.html
                            (Site_pages.msg_page
                               ?user:(Dream.session_field request "username")
                               ~title:"User Not Found"
                               ~message:
                                 ("No user was found with the username u/"
                                ^ target_username ^ ".")
                               ~alert_type:"error" ~return_url:"/" request)
                      | Error e ->
                          Dream.html
                            (Site_pages.msg_page
                               ?user:(Dream.session_field request "username")
                               ~title:"Error"
                               ~message:(Handler_support.db_error_message e)
                               ~alert_type:"error" ~return_url:"/" request)))
      | _ ->
          Dream.html
            (Site_pages.msg_page
               ?user:(Dream.session_field request "username")
               ~title:"Form Error"
               ~message:
                 "There was a problem with your form submission. Please try \
                  again."
               ~alert_type:"error" ~return_url:"/" request))

let unban_community_user_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id =
            try
              int_of_string
                (List.assoc_opt "community_id" form_data
                |> Option.value ~default:"")
            with _ -> 0
          in
          let target_user_id =
            try
              int_of_string
                (List.assoc_opt "target_user_id" form_data
                |> Option.value ~default:"")
            with _ -> 0
          in
          if community_id = 0 || target_user_id = 0 then
            Dream.respond ~status:`Bad_Request
              (Site_pages.msg_page
                 ?user:(Dream.session_field request "username")
                 ~title:"Form Error" ~message:"Invalid form data."
                 ~alert_type:"error" ~return_url:"/" request)
          else
            Dream.sql request (fun db ->
                (* Admins bypass mod-check for unban, symmetric with ban_community_user_handler —
               on CURRENT durable authority, and refusing generically when it
               cannot be established. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page
                         ?user:(Dream.session_field request "username")
                         ~title:"Error"
                         ~message:(Handler_support.db_error_message e)
                         ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin -> (
                    let%lwt is_authorized =
                      if is_admin then Lwt.return true
                      else
                        match%lwt
                          Moderator_store.is_moderator db user_id community_id
                        with
                        | Ok b -> Lwt.return b
                        | _ -> Lwt.return false
                    in
                    if not is_authorized then
                      Dream.respond ~status:`Forbidden
                        (Site_pages.msg_page
                           ?user:(Dream.session_field request "username")
                           ~title:"Access Denied"
                           ~message:"You are not a moderator of this community."
                           ~alert_type:"error" ~return_url:"/" request)
                    else
                      (* The authoritative record is loaded BEFORE mutating: the
                 Location header must come from the database slug, never the
                 submitted community_slug field (redirect/header injection),
                 and a failed lookup or unban must surface as an error rather
                 than a success-shaped redirect. *)
                      match%lwt
                        Community_store.get_community_by_id db community_id
                      with
                      | Error e ->
                          Dream.respond ~status:`Internal_Server_Error
                            (Site_pages.msg_page
                               ?user:(Dream.session_field request "username")
                               ~title:"Error"
                               ~message:(Handler_support.db_error_message e)
                               ~alert_type:"error" ~return_url:"/" request)
                      | Ok None ->
                          Dream.respond ~status:`Not_Found
                            (Site_pages.msg_page
                               ?user:(Dream.session_field request "username")
                               ~title:"Not Found"
                               ~message:"Community not found."
                               ~alert_type:"error" ~return_url:"/" request)
                      | Ok (Some community) -> (
                          let settings_url =
                            "/c/" ^ community.slug ^ "/settings?panel=bans"
                          in
                          match%lwt
                            Community_ban_store.unban_user db target_user_id
                              community_id
                          with
                          | Error e ->
                              Dream.respond ~status:`Internal_Server_Error
                                (Site_pages.msg_page
                                   ?user:
                                     (Dream.session_field request "username")
                                   ~title:"Error"
                                   ~message:(Handler_support.db_error_message e)
                                   ~alert_type:"error" ~return_url:settings_url
                                   request)
                          | Ok () -> Dream.redirect request settings_url)))
      | _ ->
          Dream.html
            (Site_pages.msg_page
               ?user:(Dream.session_field request "username")
               ~title:"Form Error"
               ~message:
                 "There was a problem with your form submission. Please try \
                  again."
               ~alert_type:"error" ~return_url:"/" request))

(* === REPORTS === *)

(* Report creation for posts & comments (chat messages out of scope). The form
   is no-JS SSR; the POST handler — not the render-time link gate — is the security
   boundary. Both handlers gate on can_view_community first (an outsider to a private
   community gets the canonical community_not_found 404 before any ban check or target
   resolution), then re-resolve the target from the trusted :slug + hidden type/id,
   verify it belongs to this community, re-run the global/community ban gates (active
   sessions outlive a ban, mirroring create_comment_handler), and disallow self-reports. *)

(* Shared post/comment target resolution. Returns the target's author id and the canonical
   return URL, or None when the target is missing / hard-deleted / not in this community.
   chat_message is rejected before this is ever called, but is handled for exhaustiveness. *)
let resolve_report_target db community_id
    (target_type : Report_store.report_target) target_id =
  match target_type with
  | Report_store.Report_post -> (
      match%lwt Post_store.get_post_by_id db target_id with
      | Ok (Some post) when post.community_id = community_id ->
          Lwt.return
            (Ok
               (Some
                  ( post.user_id,
                    post.title,
                    Post_cards.canonical_thread_path post.community_slug post.id
                      post.title )))
      | Ok _ -> Lwt.return (Ok None)
      | Error e -> Lwt.return (Error e))
  | Report_store.Report_comment -> (
      match%lwt Comment_store.get_comment_report_target db target_id with
      | Ok (Some crt) when crt.crt_community_id = community_id ->
          Lwt.return
            (Ok
               (Some
                  ( crt.crt_author_user_id,
                    crt.crt_content,
                    Post_cards.canonical_thread_path crt.crt_community_slug
                      crt.crt_post_id crt.crt_post_title )))
      | Ok _ -> Lwt.return (Ok None)
      | Error e -> Lwt.return (Error e))
  | Report_store.Report_chat_message -> Lwt.return (Ok None)

let report_form_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      let target_type_opt =
        match Dream.query request "type" with
        | Some s -> Report_store.report_target_of_string s
        | None -> None
      in
      let target_id =
        match Dream.query request "id" with
        | Some s -> ( try int_of_string s with _ -> 0)
        | None -> 0
      in
      let bad () =
        Dream.respond ~status:`Bad_Request
          (Site_pages.msg_page ?user ~title:"Invalid Report"
             ~message:"That report link is not valid." ~alert_type:"error"
             ~return_url:("/c/" ^ slug) request)
      in
      match target_type_opt with
      | Some
          ((Report_store.Report_post | Report_store.Report_comment) as
           target_type)
        when target_id > 0 ->
          Dream.sql request (fun db ->
              match%lwt Community_store.get_community_by_slug db slug with
              | Error e ->
                  Dream.respond ~status:`Internal_Server_Error
                    (Site_pages.msg_page ?user ~title:"Error"
                       ~message:(Handler_support.db_error_message e)
                       ~alert_type:"error" ~return_url:"/" request)
              | Ok None -> Community_read_gate.community_not_found ?user request
              | Ok (Some community) -> (
                  (* Private-community read gate BEFORE any ban check or target
                    resolution: an outsider must get the same 404 as a missing
                    community — a ban page or target-dependent response here
                    would confirm the community (or target) exists. *)
                  let%lwt is_admin =
                    Admin_authority.current_admin_read_override db request
                  in
                  let%lwt authorized =
                    Community_read_gate.can_view_community db ~user_id
                      ~admin_override:is_admin community
                  in
                  if not authorized then
                    Community_read_gate.community_not_found ?user request
                  else
                    (* Both ban reads sit AFTER the privacy gate above and fail
                    closed: the viewer is already authorized to see this
                    community, so a storage failure can safely be a generic
                    500 — and the form, whose policy requires a non-banned
                    reporter, is not rendered on an unknown ban state. Keeps
                    GET coherent with the POST that follows it. *)
                    match%lwt Admin_store.is_globally_banned db user_id with
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:"/" request)
                    | Ok true ->
                        Dream.respond ~status:`Forbidden
                          (Site_pages.msg_page ?user ~title:"Account Banned"
                             ~message:
                               "Your account has been permanently banned from \
                                Earde."
                             ~alert_type:"error" ~return_url:"/" request)
                    | Ok false -> (
                        match%lwt
                          Community_ban_store.is_banned db user_id community.id
                        with
                        | Error e ->
                            Dream.respond ~status:`Internal_Server_Error
                              (Site_pages.msg_page ?user ~title:"Error"
                                 ~message:(Handler_support.db_error_message e)
                                 ~alert_type:"error" ~return_url:"/" request)
                        | Ok true ->
                            Dream.respond ~status:`Forbidden
                              (Site_pages.msg_page ?user
                                 ~title:"Banned from Community"
                                 ~message:"You are banned from this community."
                                 ~alert_type:"error"
                                 ~return_url:("/c/" ^ community.slug) request)
                        | Ok false -> (
                            match%lwt
                              resolve_report_target db community.id target_type
                                target_id
                            with
                            | Error e ->
                                Dream.respond ~status:`Internal_Server_Error
                                  (Site_pages.msg_page ?user ~title:"Error"
                                     ~message:
                                       (Handler_support.db_error_message e)
                                     ~alert_type:"error" ~return_url:"/" request)
                            | Ok None -> bad ()
                            | Ok (Some (author_id, target_title, return_url)) ->
                                if author_id = user_id then
                                  Dream.respond ~status:`Forbidden
                                    (Site_pages.msg_page ?user
                                       ~title:"Cannot Report"
                                       ~message:
                                         "You cannot report your own content. \
                                          You can delete it instead."
                                       ~alert_type:"error" ~return_url request)
                                else
                                  (* Launch-chrome data, loaded only after every
                           gate above passed — a banned viewer, a foreign or
                           deleted target and a self-report never touch sections,
                           channels or the viewer's membership. Each degrades to
                           an empty list on error rather than blocking the form.
                           can_manage mirrors modlog's can_access_settings gate
                           (admin || moderator) and only picks the sidebar
                           Settings visibility; the settings handler re-checks. *)
                                  let%lwt can_manage =
                                    if is_admin then Lwt.return true
                                    else
                                      match%lwt
                                        Moderator_store.is_moderator db user_id
                                          community.id
                                      with
                                      | Ok b -> Lwt.return b
                                      | _ -> Lwt.return false
                                  in
                                  let%lwt sections =
                                    if community.sections_enabled then
                                      match%lwt
                                        Section_store.get_sections_by_community
                                          db community.id
                                      with
                                      | Ok secs -> Lwt.return secs
                                      | Error _ -> Lwt.return []
                                    else Lwt.return []
                                  in
                                  let%lwt channels =
                                    match%lwt
                                      Channel_store.get_channels_by_community db
                                        community.id
                                    with
                                    | Ok cs -> Lwt.return cs
                                    | Error _ -> Lwt.return []
                                  in
                                  let%lwt rail_communities =
                                    match%lwt
                                      Membership_store.get_user_communities db
                                        user_id
                                    with
                                    | Ok cs -> Lwt.return cs
                                    | Error _ -> Lwt.return []
                                  in
                                  Dream.html
                                    (Moderation_pages.report_form_page ?user
                                       ~rail_communities ~channels ~sections
                                       ~can_manage ~community ~target_type
                                       ~target_id ~target_title ~return_url
                                       request)))))
      | _ -> bad ())

let create_report_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data -> (
          let get n = Option.value ~default:"" (List.assoc_opt n form_data) in
          let target_type_opt =
            Report_store.report_target_of_string (get "target_type")
          in
          let reason_opt =
            Report_store.report_reason_of_string (get "reason")
          in
          let target_id = try int_of_string (get "target_id") with _ -> 0 in
          (* Cap details server-side; the form's maxlength is advisory only. *)
          let details =
            match String.trim (get "details") with
            | "" -> None
            | d ->
                Some (if String.length d > 1000 then String.sub d 0 1000 else d)
          in
          match (target_type_opt, reason_opt) with
          | ( Some
                ((Report_store.Report_post | Report_store.Report_comment) as
                 target_type),
              Some reason )
            when target_id > 0 ->
              Dream.sql request (fun db ->
                  match%lwt Community_store.get_community_by_slug db slug with
                  | Error e ->
                      Dream.respond ~status:`Internal_Server_Error
                        (Site_pages.msg_page ?user ~title:"Error"
                           ~message:(Handler_support.db_error_message e)
                           ~alert_type:"error" ~return_url:"/" request)
                  | Ok None ->
                      Community_read_gate.community_not_found ?user request
                  | Ok (Some community) -> (
                      (* Same private-community read gate as the GET form, re-proved
                        from server-side state — the POST must not trust that the
                        viewer ever loaded the form. Denial happens before target
                        resolution so valid and invalid private target ids are
                        indistinguishable and no report row is ever inserted. *)
                      let%lwt is_admin =
                        Admin_authority.current_admin_read_override db request
                      in
                      let%lwt authorized =
                        Community_read_gate.can_view_community db ~user_id
                          ~admin_override:is_admin community
                      in
                      if not authorized then
                        Community_read_gate.community_not_found ?user request
                      else
                        (* Ban gates, still after the privacy gate above and now
                        fail-closed: no report row may be inserted while the
                        reporter's ban state is unknown. *)
                        match%lwt Admin_store.is_globally_banned db user_id with
                        | Error e ->
                            Dream.respond ~status:`Internal_Server_Error
                              (Site_pages.msg_page ?user ~title:"Error"
                                 ~message:(Handler_support.db_error_message e)
                                 ~alert_type:"error" ~return_url:"/" request)
                        | Ok true ->
                            Dream.respond ~status:`Forbidden
                              (Site_pages.msg_page ?user ~title:"Account Banned"
                                 ~message:
                                   "Your account has been permanently banned \
                                    from Earde."
                                 ~alert_type:"error" ~return_url:"/" request)
                        | Ok false -> (
                            match%lwt
                              Community_ban_store.is_banned db user_id
                                community.id
                            with
                            | Error e ->
                                Dream.respond ~status:`Internal_Server_Error
                                  (Site_pages.msg_page ?user ~title:"Error"
                                     ~message:
                                       (Handler_support.db_error_message e)
                                     ~alert_type:"error" ~return_url:"/" request)
                            | Ok true ->
                                Dream.respond ~status:`Forbidden
                                  (Site_pages.msg_page ?user
                                     ~title:"Banned from Community"
                                     ~message:
                                       "You are banned from this community."
                                     ~alert_type:"error"
                                     ~return_url:("/c/" ^ community.slug)
                                     request)
                            | Ok false -> (
                                (* Re-resolve from the trusted slug — never trust a client-supplied community_id. *)
                                match%lwt
                                  resolve_report_target db community.id
                                    target_type target_id
                                with
                                | Error e ->
                                    Dream.respond ~status:`Internal_Server_Error
                                      (Site_pages.msg_page ?user ~title:"Error"
                                         ~message:
                                           (Handler_support.db_error_message e)
                                         ~alert_type:"error"
                                         ~return_url:("/c/" ^ community.slug)
                                         request)
                                | Ok None ->
                                    Dream.respond ~status:`Not_Found
                                      (Site_pages.msg_page ?user
                                         ~title:"Not Found"
                                         ~message:
                                           "That content is no longer \
                                            available."
                                         ~alert_type:"error"
                                         ~return_url:("/c/" ^ community.slug)
                                         request)
                                | Ok (Some (author_id, _title, return_url)) -> (
                                    if author_id = user_id then
                                      Dream.respond ~status:`Forbidden
                                        (Site_pages.msg_page ?user
                                           ~title:"Cannot Report"
                                           ~message:
                                             "You cannot report your own \
                                              content. You can delete it \
                                              instead."
                                           ~alert_type:"error" ~return_url
                                           request)
                                    else
                                      match%lwt
                                        Report_store.create_report db
                                          ~community_id:community.id
                                          ~reporter_user_id:user_id ~target_type
                                          ~target_id:(Int64.of_int target_id)
                                          ~target_author_user_id:
                                            (Some author_id) ~reason ~details
                                      with
                                      | Ok (`Created _) ->
                                          Dream.html
                                            (Site_pages.msg_page ?user
                                               ~title:"Report submitted"
                                               ~message:
                                                 "Thanks — a moderator will \
                                                  review this."
                                               ~alert_type:"success" ~return_url
                                               request)
                                      | Ok `Duplicate ->
                                          Dream.html
                                            (Site_pages.msg_page ?user
                                               ~title:"Already reported"
                                               ~message:
                                                 "You've already reported this \
                                                  item. A moderator will \
                                                  review it."
                                               ~alert_type:"info" ~return_url
                                               request)
                                      | Error e ->
                                          Dream.respond
                                            ~status:`Internal_Server_Error
                                            (Site_pages.msg_page ?user
                                               ~title:"Error"
                                               ~message:
                                                 (Handler_support
                                                  .db_error_message e)
                                               ~alert_type:"error" ~return_url
                                               request))))))
          | _ ->
              Dream.respond ~status:`Bad_Request
                (Site_pages.msg_page ?user ~title:"Invalid Report"
                   ~message:"That report could not be processed."
                   ~alert_type:"error" ~return_url:("/c/" ^ slug) request))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page ?user ~title:"Form Error"
               ~message:
                 "There was a problem with your form submission. Please try \
                  again."
               ~alert_type:"error" ~return_url:("/c/" ^ slug) request))

(* Read-only community mod queue. PRIVATE: gated to M/TM/A with the exact
   community_settings_handler idiom (admin bypass, else is_moderator) — never the public
   modlog_handler shape, since this exposes reporter identities. No mutation. *)
let reports_queue_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      (* Default open; an unknown ?status= falls back to open (least surprising). *)
      let status =
        match Dream.query request "status" with
        | Some s -> (
            match Report_store.report_status_of_string s with
            | Some st -> st
            | None -> Report_store.Report_open)
        | None -> Report_store.Report_open
      in
      Dream.sql request (fun db ->
          match%lwt Community_store.get_community_by_slug db slug with
          | Error e ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page ?user ~title:"Error"
                   ~message:(Handler_support.db_error_message e)
                   ~alert_type:"error" ~return_url:"/" request)
          | Ok None ->
              Dream.respond ~status:`Not_Found
                (Site_pages.msg_page ?user ~title:"Not Found"
                   ~message:"This community does not exist." ~alert_type:"error"
                   ~return_url:"/" request)
          | Ok (Some community) -> (
              (* Reporter identities are private data: the admin disjunct is
               CURRENT durable authority, and an unanswerable lookup is the
               generic failure rather than a granted override. *)
              match%lwt
                Admin_authority.current_admin_bool db request
              with
              | Error e ->
                  Dream.respond ~status:`Internal_Server_Error
                    (Site_pages.msg_page ?user ~title:"Error"
                       ~message:(Handler_support.db_error_message e)
                       ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
              | Ok is_admin -> (
                  let%lwt is_authorized =
                    if is_admin then Lwt.return true
                    else
                      match%lwt
                        Moderator_store.is_moderator db user_id community.id
                      with
                      | Ok b -> Lwt.return b
                      | _ -> Lwt.return false
                  in
                  if not is_authorized then
                    Dream.respond ~status:`Forbidden
                      (Site_pages.msg_page ?user ~title:"Access Denied"
                         ~message:"You must be a moderator to view reports."
                         ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                  else
                    (* Launch-chrome data, loaded only after the M/TM/A
                 authorization above — a denied request never touches sections,
                 channels or the viewer's membership. Each degrades to an empty
                 list on error rather than blocking the queue. Same plumbing as
                 the sibling converted management routes. *)
                    let%lwt sections =
                      if community.sections_enabled then
                        match%lwt
                          Section_store.get_sections_by_community db
                            community.id
                        with
                        | Ok secs -> Lwt.return secs
                        | Error _ -> Lwt.return []
                      else Lwt.return []
                    in
                    let%lwt channels =
                      match%lwt
                        Channel_store.get_channels_by_community db community.id
                      with
                      | Ok cs -> Lwt.return cs
                      | Error _ -> Lwt.return []
                    in
                    let%lwt rail_communities =
                      match%lwt
                        Membership_store.get_user_communities db user_id
                      with
                      | Ok cs -> Lwt.return cs
                      | Error _ -> Lwt.return []
                    in
                    (* Read-only role lookup for the shared settings shell's nav:
                 display-gates the top-mod/admin entries (Network group,
                 Manage moderators) exactly like the settings hub. Every
                 linked route still reauthorizes — this changes no
                 permission. *)
                    let%lwt is_top_mod =
                      match%lwt
                        Moderator_store.get_moderator_role db user_id
                          community.id
                      with
                      | Ok (Some "top_mod") -> Lwt.return true
                      | _ -> Lwt.return false
                    in
                    match%lwt
                      Report_store.get_reports_by_community db community.id
                        ~status
                    with
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:("/c/" ^ slug)
                             request)
                    | Ok reports ->
                        (* Bounded per-row context+preview lookup (read-only MVP): reuse
                      resolve_report_target so deleted / foreign / chat targets degrade to no
                      preview. Capped so a flooded queue can't fan out into an unbounded N+1. *)
                        let preview_cap = 100 in
                        let rec take n = function
                          | [] -> []
                          | _ when n <= 0 -> []
                          | x :: xs -> x :: take (n - 1) xs
                        in
                        let%lwt previews =
                          Lwt_list.filter_map_s
                            (fun (r : Report_store.report_row) ->
                              match%lwt
                                resolve_report_target db community.id
                                  r.target_type (Int64.to_int r.target_id)
                              with
                              | Ok (Some (_author, preview, url)) ->
                                  Lwt.return (Some (r.id, (url, preview)))
                              | _ -> Lwt.return None)
                            (take preview_cap reports)
                        in
                        Dream.html
                          (Moderation_pages.reports_queue_page ?user
                             ~rail_communities ~is_admin ~is_top_mod ~channels
                             ~sections ~community ~status ~reports ~previews
                             request))))

(* Resolve an open report (dismiss / mark action-taken) and write a modlog entry.
   Shared by dismiss_report_handler and action_report_handler. Both gate exactly like the
   read-only queue (M/TM/A), resolve the community from the trusted :slug (never a form field),
   verify the report belongs to THIS community, and only mutate while status=open — re-resolving
   an already-closed report is a friendly redirect, not a double-log or a 500.

   The modlog target_id is the report_id (always INTEGER), never report.target_id, which may be
   a chat-message BIGINT that mod_actions.target_id (INTEGER) cannot hold. *)
let resolve_report_action request ~new_status ~action_kind ~action_type
    ~default_reason =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      let report_id =
        try int_of_string (Dream.param request "report_id") with _ -> 0
      in
      let reports_url = "/c/" ^ slug ^ "/reports" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          (* Cap the optional note so a hostile/huge paste can't bloat the row or the modlog. *)
          let note =
            match List.assoc_opt "resolution_note" form_data with
            | Some s ->
                let t = String.trim s in
                if t = "" then None
                else
                  Some
                    (if String.length t > 1000 then String.sub t 0 1000 else t)
            | None -> None
          in
          Dream.sql request (fun db ->
              match%lwt Community_store.get_community_by_slug db slug with
              | Error e ->
                  Dream.respond ~status:`Internal_Server_Error
                    (Site_pages.msg_page ?user ~title:"Error"
                       ~message:(Handler_support.db_error_message e)
                       ~alert_type:"error" ~return_url:"/" request)
              | Ok None ->
                  Dream.respond ~status:`Not_Found
                    (Site_pages.msg_page ?user ~title:"Not Found"
                       ~message:"This community does not exist."
                       ~alert_type:"error" ~return_url:"/" request)
              | Ok (Some community) -> (
                  (* Same M/TM/A gate as the queue, on CURRENT durable admin
                    authority; an unanswerable lookup resolves no report and
                    writes no modlog entry. *)
                  match%lwt
                    Admin_authority.current_admin_bool db request
                  with
                  | Error e ->
                      Dream.respond ~status:`Internal_Server_Error
                        (Site_pages.msg_page ?user ~title:"Error"
                           ~message:(Handler_support.db_error_message e)
                           ~alert_type:"error" ~return_url:reports_url request)
                  | Ok is_admin -> (
                      let%lwt is_authorized =
                        if is_admin then Lwt.return true
                        else
                          match%lwt
                            Moderator_store.is_moderator db user_id community.id
                          with
                          | Ok b -> Lwt.return b
                          | _ -> Lwt.return false
                      in
                      if not is_authorized then
                        Dream.respond ~status:`Forbidden
                          (Site_pages.msg_page ?user ~title:"Access Denied"
                             ~message:
                               "You must be a moderator to resolve reports."
                             ~alert_type:"error" ~return_url:("/c/" ^ slug)
                             request)
                      else
                        match%lwt
                          Report_store.get_report_by_id db report_id
                        with
                        | Error e ->
                            Dream.respond ~status:`Internal_Server_Error
                              (Site_pages.msg_page ?user ~title:"Error"
                                 ~message:(Handler_support.db_error_message e)
                                 ~alert_type:"error" ~return_url:reports_url
                                 request)
                        | Ok None ->
                            Dream.respond ~status:`Not_Found
                              (Site_pages.msg_page ?user ~title:"Not Found"
                                 ~message:"That report does not exist."
                                 ~alert_type:"error" ~return_url:reports_url
                                 request)
                        | Ok (Some report) -> (
                            if
                              (* Bind to the slug's community: a report_id from another community must not
                           be mutable under this community's mod authority. *)
                              report.community_id <> community.id
                            then
                              Dream.respond ~status:`Not_Found
                                (Site_pages.msg_page ?user ~title:"Not Found"
                                   ~message:
                                     "That report does not belong to this \
                                      community."
                                   ~alert_type:"error" ~return_url:reports_url
                                   request)
                            else if report.status <> Report_store.Report_open
                            then
                              (* Already resolved (possibly by another mod): no mutation, bounce to the
                             tab it now lives in. *)
                              Dream.redirect request
                                (reports_url ^ "?status="
                                ^ Report_store.report_status_to_string
                                    report.status)
                            else
                              match%lwt
                                Report_store.resolve_report db report_id
                                  ~resolver_user_id:user_id ~status:new_status
                                  ~action_kind ~note
                              with
                              | Error e ->
                                  Dream.respond ~status:`Internal_Server_Error
                                    (Site_pages.msg_page ?user ~title:"Error"
                                       ~message:
                                         (Handler_support.db_error_message e)
                                       ~alert_type:"error"
                                       ~return_url:reports_url request)
                              | Ok () ->
                                  let reason =
                                    match note with
                                    | Some n -> n
                                    | None -> default_reason report_id
                                  in
                                  let%lwt _ =
                                    Mod_log_store.log_action db community.id
                                      user_id action_type (Some report_id)
                                      reason
                                  in
                                  Dream.redirect request
                                    (reports_url ^ "?status="
                                    ^ Report_store.report_status_to_string
                                        new_status)))))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page ?user ~title:"Form Error"
               ~message:"Invalid form submission." ~alert_type:"error"
               ~return_url:reports_url request))

(* Dismiss: report had no actionable merit. No content action, so action_kind stays None. *)
let dismiss_report_handler request =
  resolve_report_action request ~new_status:Report_store.Report_dismissed
    ~action_kind:None ~action_type:"dismiss_report" ~default_reason:(fun id ->
      Printf.sprintf "Dismissed report #%d" id)

(* Mark action taken: the mod acted on this report. This slice does NOT remove content or ban
   anyone, so the recorded action_kind is Report_other_action — never removed_content/banned_author,
   which would claim an action that did not happen. Content removal is a later slice. *)
let action_report_handler request =
  resolve_report_action request ~new_status:Report_store.Report_action_taken
    ~action_kind:(Some Report_store.Report_other_action)
    ~action_type:"resolve_report" ~default_reason:(fun id ->
      Printf.sprintf "Marked report #%d as action taken" id)

(* === MANAGE MODS === *)

let manage_mods_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      Dream.sql request (fun db ->
          (* Only the admin disjunct changes: CURRENT durable authority, and a
           generic failure — never an override — when it cannot be
           established. The top-mod arm is untouched. *)
          match%lwt Admin_authority.current_admin_bool db request with
          | Error e ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page ?user ~title:"Error"
                   ~message:(Handler_support.db_error_message e)
                   ~alert_type:"error" ~return_url:"/" request)
          | Ok is_admin -> (
              match%lwt Community_store.get_community_by_slug db slug with
              | Ok (Some community) -> (
                  let%lwt role_res =
                    Moderator_store.get_moderator_role db user_id community.id
                  in
                  let current_user_role =
                    match role_res with Ok r -> r | _ -> None
                  in
                  let is_authorized =
                    is_admin || current_user_role = Some "top_mod"
                  in
                  if not is_authorized then
                    Dream.respond ~status:`Forbidden
                      (Site_pages.msg_page ?user ~title:"Access Denied"
                         ~message:
                           "Only Top Mods and Admins can manage moderators."
                         ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                  else
                    (* Launch-chrome data, loaded only after the TM/A
                 authorization above — a denied request never touches sections,
                 channels or the viewer's membership. Each degrades to an empty
                 list on error rather than blocking the roster. Same plumbing as
                 the sibling converted management routes. *)
                    let%lwt sections =
                      if community.sections_enabled then
                        match%lwt
                          Section_store.get_sections_by_community db
                            community.id
                        with
                        | Ok secs -> Lwt.return secs
                        | Error _ -> Lwt.return []
                      else Lwt.return []
                    in
                    let%lwt channels =
                      match%lwt
                        Channel_store.get_channels_by_community db community.id
                      with
                      | Ok cs -> Lwt.return cs
                      | Error _ -> Lwt.return []
                    in
                    let%lwt rail_communities =
                      match%lwt
                        Membership_store.get_user_communities db user_id
                      with
                      | Ok cs -> Lwt.return cs
                      | Error _ -> Lwt.return []
                    in
                    match%lwt
                      Moderator_store.get_community_mods_with_roles db
                        community.id
                    with
                    | Ok mods ->
                        Dream.html
                          (Moderation_pages.manage_mods_page ?user
                             ~rail_communities ~is_admin ~current_user_role
                             ~channels ~sections ~community ~mods request)
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:("/c/" ^ slug)
                             request))
              | Ok None ->
                  Dream.respond ~status:`Not_Found
                    (Site_pages.msg_page ?user ~title:"Not Found"
                       ~message:"This community does not exist."
                       ~alert_type:"error" ~return_url:"/" request)
              | Error e ->
                  Dream.respond ~status:`Internal_Server_Error
                    (Site_pages.msg_page ?user ~title:"Error"
                       ~message:(Handler_support.db_error_message e)
                       ~alert_type:"error" ~return_url:"/" request)))

let manage_mods_add_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_username =
            String.trim
              (List.assoc_opt "username" form_data |> Option.value ~default:"")
          in
          if target_username = "" then
            Dream.respond ~status:`Bad_Request
              (Site_pages.msg_page ?user ~title:"Form Error"
                 ~message:"Username is required." ~alert_type:"error"
                 ~return_url:("/c/" ^ slug ^ "/manage-mods")
                 request)
          else
            Dream.sql request (fun db ->
                (* Only the admin disjunct changes: CURRENT durable authority, and
               a generic failure — never an override — when it cannot be
               established. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user ~title:"Error"
                         ~message:(Handler_support.db_error_message e)
                         ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin -> (
                    match%lwt Community_store.get_community_by_slug db slug with
                    | Ok (Some community) -> (
                        let%lwt role_res =
                          Moderator_store.get_moderator_role db user_id
                            community.id
                        in
                        let current_user_role =
                          match role_res with Ok r -> r | _ -> None
                        in
                        let is_authorized =
                          is_admin || current_user_role = Some "top_mod"
                        in
                        if not is_authorized then
                          Dream.respond ~status:`Forbidden
                            (Site_pages.msg_page ?user ~title:"Access Denied"
                               ~message:
                                 "Only Top Mods and Admins can add moderators."
                               ~alert_type:"error"
                               ~return_url:("/c/" ^ slug ^ "/manage-mods")
                               request)
                        else
                          match%lwt
                            User_store.get_user_by_username db target_username
                          with
                          | Ok (Some target_user) ->
                              let%lwt _ =
                                Moderator_store.add_moderator db target_user.id
                                  community.id
                              in
                              Dream.redirect request
                                ("/c/" ^ slug ^ "/manage-mods")
                          | Ok None ->
                              Dream.html
                                (Site_pages.msg_page ?user
                                   ~title:"User Not Found"
                                   ~message:
                                     ("No user found: u/" ^ target_username)
                                   ~alert_type:"error"
                                   ~return_url:("/c/" ^ slug ^ "/manage-mods")
                                   request)
                          | Error e ->
                              Dream.html
                                (Site_pages.msg_page ?user ~title:"Error"
                                   ~message:(Handler_support.db_error_message e)
                                   ~alert_type:"error" ~return_url:"/" request))
                    | Ok None ->
                        Dream.respond ~status:`Not_Found
                          (Site_pages.msg_page ?user ~title:"Not Found"
                             ~message:"Community not found." ~alert_type:"error"
                             ~return_url:"/" request)
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:"/" request)))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page ?user ~title:"Form Error"
               ~message:"Invalid form submission." ~alert_type:"error"
               ~return_url:("/c/" ^ slug ^ "/manage-mods")
               request))

let manage_mods_promote_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_user_id =
            try
              int_of_string
                (List.assoc_opt "target_user_id" form_data
                |> Option.value ~default:"")
            with _ -> 0
          in
          if target_user_id = 0 then
            Dream.respond ~status:`Bad_Request
              (Site_pages.msg_page ?user ~title:"Form Error"
                 ~message:"Invalid user reference." ~alert_type:"error"
                 ~return_url:("/c/" ^ slug ^ "/manage-mods")
                 request)
          else
            Dream.sql request (fun db ->
                (* Only the admin disjunct changes: CURRENT durable authority, and
               a generic failure — never an override — when it cannot be
               established. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user ~title:"Error"
                         ~message:(Handler_support.db_error_message e)
                         ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin -> (
                    match%lwt Community_store.get_community_by_slug db slug with
                    | Ok (Some community) -> (
                        let%lwt role_res =
                          Moderator_store.get_moderator_role db user_id
                            community.id
                        in
                        let current_user_role =
                          match role_res with Ok r -> r | _ -> None
                        in
                        let is_authorized =
                          is_admin || current_user_role = Some "top_mod"
                        in
                        if not is_authorized then
                          Dream.respond ~status:`Forbidden
                            (Site_pages.msg_page ?user ~title:"Access Denied"
                               ~message:
                                 "Only Top Mods and Admins can promote \
                                  moderators."
                               ~alert_type:"error"
                               ~return_url:("/c/" ^ slug ^ "/manage-mods")
                               request)
                        else
                          match%lwt
                            Moderator_store.promote_to_top_mod db target_user_id
                              community.id
                          with
                          | Ok () ->
                              Dream.redirect request
                                ("/c/" ^ slug ^ "/manage-mods")
                          | Error (Moderator_store.Promotion_refused msg) ->
                              (* Fixed domain refusals (not a moderator, already Top
                          Mod, seat cap) stay user-visible verbatim. *)
                              Dream.html
                                (Site_pages.msg_page ?user
                                   ~title:"Promotion Failed" ~message:msg
                                   ~alert_type:"error"
                                   ~return_url:("/c/" ^ slug ^ "/manage-mods")
                                   request)
                          | Error (Moderator_store.Promotion_storage_error e) ->
                              Dream.html
                                (Site_pages.msg_page ?user
                                   ~title:"Promotion Failed"
                                   ~message:(Handler_support.db_error_message e)
                                   ~alert_type:"error"
                                   ~return_url:("/c/" ^ slug ^ "/manage-mods")
                                   request))
                    | Ok None ->
                        Dream.respond ~status:`Not_Found
                          (Site_pages.msg_page ?user ~title:"Not Found"
                             ~message:"Community not found." ~alert_type:"error"
                             ~return_url:"/" request)
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:"/" request)))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page ?user ~title:"Form Error"
               ~message:"Invalid form submission." ~alert_type:"error"
               ~return_url:("/c/" ^ slug ^ "/manage-mods")
               request))

let manage_mods_remove_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let target_user_id =
            try
              int_of_string
                (List.assoc_opt "target_user_id" form_data
                |> Option.value ~default:"")
            with _ -> 0
          in
          if target_user_id = 0 then
            Dream.respond ~status:`Bad_Request
              (Site_pages.msg_page ?user ~title:"Form Error"
                 ~message:"Invalid user reference." ~alert_type:"error"
                 ~return_url:("/c/" ^ slug ^ "/manage-mods")
                 request)
          else
            Dream.sql request (fun db ->
                (* Only the admin disjunct changes: CURRENT durable authority, and
               a generic failure — never an override — when it cannot be
               established. It also governs the top-mod-cannot-remove-a-top-mod
               guard below, so a stale claim cannot unseat a Top Mod. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user ~title:"Error"
                         ~message:(Handler_support.db_error_message e)
                         ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin -> (
                    match%lwt Community_store.get_community_by_slug db slug with
                    | Ok (Some community) -> (
                        let%lwt role_res =
                          Moderator_store.get_moderator_role db user_id
                            community.id
                        in
                        let current_user_role =
                          match role_res with Ok r -> r | _ -> None
                        in
                        let is_authorized =
                          is_admin || current_user_role = Some "top_mod"
                        in
                        if not is_authorized then
                          Dream.respond ~status:`Forbidden
                            (Site_pages.msg_page ?user ~title:"Access Denied"
                               ~message:
                                 "Only Top Mods and Admins can remove \
                                  moderators."
                               ~alert_type:"error"
                               ~return_url:("/c/" ^ slug ^ "/manage-mods")
                               request)
                        else
                          (* Re-fetch target role server-side: prevents a top_mod from removing
                     another top_mod by manipulating the form — TOCTOU guard. *)
                          match%lwt
                            Moderator_store.get_moderator_role db target_user_id
                              community.id
                          with
                          | Ok (Some "top_mod") when not is_admin ->
                              Dream.html
                                (Site_pages.msg_page ?user
                                   ~title:"Action Denied"
                                   ~message:
                                     "Top Mods cannot remove other Top Mods. \
                                      Only an admin can do this."
                                   ~alert_type:"error"
                                   ~return_url:("/c/" ^ slug ^ "/manage-mods")
                                   request)
                          | Ok None ->
                              Dream.html
                                (Site_pages.msg_page ?user
                                   ~title:"Not a Moderator"
                                   ~message:
                                     "That user is not a moderator of this \
                                      community."
                                   ~alert_type:"error"
                                   ~return_url:("/c/" ^ slug ^ "/manage-mods")
                                   request)
                          | Ok _ -> (
                              match%lwt
                                Moderator_store.get_community_mods_with_roles db
                                  community.id
                              with
                              | Ok mods when List.length mods > 1 ->
                                  let%lwt _ =
                                    Moderator_store.remove_moderator db
                                      target_user_id community.id
                                  in
                                  Dream.redirect request
                                    ("/c/" ^ slug ^ "/manage-mods")
                              | Ok _ ->
                                  Dream.html
                                    (Site_pages.msg_page ?user
                                       ~title:"Cannot Remove"
                                       ~message:
                                         "You cannot remove the last moderator \
                                          of a community."
                                       ~alert_type:"error"
                                       ~return_url:
                                         ("/c/" ^ slug ^ "/manage-mods")
                                       request)
                              | Error e ->
                                  Dream.html
                                    (Site_pages.msg_page ?user ~title:"Error"
                                       ~message:
                                         (Handler_support.db_error_message e)
                                       ~alert_type:"error" ~return_url:"/"
                                       request))
                          | Error e ->
                              Dream.html
                                (Site_pages.msg_page ?user ~title:"Error"
                                   ~message:(Handler_support.db_error_message e)
                                   ~alert_type:"error" ~return_url:"/" request))
                    | Ok None ->
                        Dream.respond ~status:`Not_Found
                          (Site_pages.msg_page ?user ~title:"Not Found"
                             ~message:"Community not found." ~alert_type:"error"
                             ~return_url:"/" request)
                    | Error e ->
                        Dream.respond ~status:`Internal_Server_Error
                          (Site_pages.msg_page ?user ~title:"Error"
                             ~message:(Handler_support.db_error_message e)
                             ~alert_type:"error" ~return_url:"/" request)))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page ?user ~title:"Form Error"
               ~message:"Invalid form submission." ~alert_type:"error"
               ~return_url:("/c/" ^ slug ^ "/manage-mods")
               request))
