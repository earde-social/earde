let community_settings_handler request =
  let slug = Dream.param request "slug" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      Dream.sql request (fun db ->
        (* The admin disjunct of the gate below is CURRENT durable authority,
           never the session's cached claim. An unanswerable lookup is the
           generic failure — it never falls through as "admin". *)
        match%lwt Admin_authority.current_admin_bool db request with
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
        | Ok is_admin ->
        match%lwt Community_store.get_community_by_slug db slug with
        | Ok (Some community) ->
            (* Admins bypass the mod check — they have global authority over settings.
               is_moderator is still consulted for non-admins to keep the ACL simple. *)
            let%lwt is_authorized =
              if is_admin then Lwt.return true
              else (match%lwt Moderator_store.is_moderator db user_id community.id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not is_authorized then
              Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"You must be a moderator to access this page." ~alert_type:"error" ~return_url:"/" request)
            else
              (match%lwt Moderator_store.get_community_moderators db community.id with
              | Ok mods ->
                  (match%lwt Community_ban_store.get_banned_users db community.id with
                  | Ok banned_users ->
                      let%lwt sections =
                        if community.sections_enabled then
                          (match%lwt Section_store.get_sections_by_community db community.id with
                           | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
                        else Lwt.return []
                      in
                      (* Live chat channels (incl. archived) so the hub can list + manage them. *)
                      let%lwt channels =
                        match%lwt Channel_store.get_channels_by_community db community.id with
                        | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                      in
                      (* Read-only role lookup: display-gates the Moderation tools (downvote toggle)
                         to top_mod/admin so it stays hidden from regular mods exactly as it was on
                         the old public home. Authorization is still enforced by
                         toggle_downvotes_handler — this changes no settings-view permission. *)
                      let%lwt is_top_mod =
                        match%lwt Moderator_store.get_moderator_role db user_id community.id with
                        | Ok (Some "top_mod") -> Lwt.return true
                        | _ -> Lwt.return false
                      in
                      (* Cheap COUNT for the settings hub "Reports (N open)" affordance; a
                         failure degrades to 0 rather than blocking the whole settings page. *)
                      let%lwt open_reports_count =
                        match%lwt Report_store.count_open_reports db community.id with
                        | Ok n -> Lwt.return n | Error _ -> Lwt.return 0
                      in
                      (* Slice F: the community_members allow-list for the member-management card.
                         A lookup failure degrades to an empty list rather than blocking settings. *)
                      let%lwt members =
                        match%lwt Membership_store.get_community_members db community.id with
                        | Ok m -> Lwt.return m | Error _ -> Lwt.return []
                      in
                      (* Global-rail parity: the viewer's joined communities, in the same
                         stable order the feed/overview/channel/section/thread handlers
                         load. Queried only after the settings authorization above
                         succeeded, so a denied or anonymous request never touches
                         membership data; a failure degrades to the no-data rail rather
                         than blocking settings. Ordering, dedup, and the active marker
                         stay owned by the shared launch doc builder. *)
                      let%lwt rail_communities =
                        match%lwt Membership_store.get_user_communities db user_id with
                        | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
                      in
                      (* Connected-project management: loaded only after the settings
                         authorization above succeeded, and only for the same top-mod/admin
                         surface that already gates the project-home request queue. There is
                         no second accepted-project query — this is the existing public
                         read model, rendered with removal controls. *)
                      Community_handlers.with_settings_connected_projects db ?user request
                        ~community_slug:community.slug
                        ~authorized:(is_top_mod || is_admin)
                        ~removal_allowed:
                          (not
                             (community.is_network_community
                             && community.onboarding_state = Community_types.Community_draft))
                        (fun connected_projects ->
                      Dream.html (Community_settings_pages.community_settings_page ?user ~connected_projects ~rail_communities ~is_admin ~is_top_mod ~open_reports_count ~community ~mods ~banned_users ~members ~sections ~channels request))
                  | Error e -> Dream.html (Handler_support.db_error_message e))
              | Error e -> Dream.html (Handler_support.db_error_message e))
        | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
      )

(* === NETWORK-COMMUNITY LEGACY MUTATION GUARDS ===

   A provisioned network community is configured only through the canonical
   setup flow: GET /c/:slug/setup reviews the draft and the future
   POST /c/:slug/publish commits identity and exposure together, atomically.
   None of the legacy settings mutations below may stand in for it, and a
   forged direct POST must fail closed *before* any write rather than
   becoming a database CHECK violation rendered back as "Database error: …"
   (the scoped communities_network_* constraints would otherwise echo a
   constraint name to the client).

   These guards are deliberately narrow: they test durable columns on the
   authoritative record the handler has already loaded and authorized, they
   never write, and a legacy (non-network) community reaches exactly the code
   it always did. *)

let is_network_setup_draft (community : Community_types.community) =
  community.is_network_community
  && community.onboarding_state = Community_types.Community_draft

(* The canonical community-identity policy, applied to a *published* network
   community's proposed description before the legacy detail write. The
   policy is not restated here: the community's own stored name and slug ride
   along so the frozen parser decides all three together, exactly as the
   provisioning store persisted them. [Error] means fail closed — the write
   never happens — and a canonical [Ok] value is what gets stored, so the
   scoped database CHECK is a backstop rather than the enforcement point. *)
let canonical_network_description (community : Community_types.community) ~raw_description =
  match
    Project_home_provisioning_form.of_fields
      [ ("community_name", community.name);
        ("community_slug", community.slug);
        ("community_description", raw_description)
      ]
  with
  | Ok identity ->
      Ok (Project_home_provisioning_form.community_description identity)
  | Error _ -> Error ()

let update_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let community_id = try int_of_string (get_field "community_id") with _ -> 0 in
          if community_id = 0 then Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else
          (* Empty string → None: lets mods clear a field without sending NULL hacks. *)
          let str_opt s = let t = String.trim s in if t = "" then None else Some t in
          let description = str_opt (get_field "description") in
          let rules       = str_opt (get_field "rules") in
          (* Only the file parts are read here. A submitted avatar/banner URL is
             deliberately never consulted: the no-upload fallback comes from the
             community row loaded below. *)
          let avatar_bytes = get_field "avatar_url" in
          let banner_bytes = get_field "banner_url" in
          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Re-verify authority on every mutation — same TOCTOU guard as add_mod.
               The admin disjunct is CURRENT durable authority; an unanswerable
               lookup refuses before any image work or write. *)
            match%lwt Admin_authority.current_admin_bool db request with
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok is_admin ->
            let%lwt authorized =
              if is_admin then Lwt.return true
              else (match%lwt Moderator_store.is_moderator db user_id community_id with
                | Ok b -> Lwt.return b
                | _ -> Lwt.return false)
            in
            if not authorized then
              Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Access Denied" ~message:"You must be a moderator to perform this action." ~alert_type:"error" ~return_url:"/" request)
            else
            (* The community record is loaded up front because every redirect
               and return URL below must be built from the authoritative
               database slug — the submitted community_slug field is
               attacker-controlled and reached the Location header. *)
            match%lwt Community_store.get_community_by_id db community_id with
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok loaded ->
            let settings_url =
              match loaded with
              | Some c -> "/c/" ^ c.slug ^ "/settings"
              | None -> "/"
            in
            (* Image processing runs only AFTER the moderator check. It used
               to run before it, so any authenticated user could force two
               full ImageMagick conversions and leave two files in
               static/uploads for any community id, then be told "Access
               Denied" — the work and the storage happened regardless. *)
            let%lwt avatar_result =
              Image_processing.process_image_upload ~db ~ip:(Dream.client request)
                ~purpose:Image_upload.Community_avatar avatar_bytes
            in
            let%lwt banner_result =
              Image_processing.process_image_upload ~db ~ip:(Dream.client request)
                ~purpose:Image_upload.Community_banner banner_bytes
            in
            match avatar_result, banner_result with
            | Error e, _ | _, Error e ->
                Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Image Error" ~message:e ~alert_type:"error" ~return_url:settings_url request)
            | Ok new_avatar, Ok new_banner ->
              (* new_avatar/new_banner are None when no file was submitted. The
                 fallback is then the community's OWN stored value, read from
                 [loaded] above — never a URL the client sent. The form used to
                 round-trip both through hidden inputs, which let any moderator
                 store an arbitrary external URL: every visitor of the public
                 community page, including anonymous ones, would then fetch it,
                 handing a third party their IP, User-Agent and Referer. A
                 community that matched no id has no stored value and no write
                 to make, so [None] keeps that path the no-op it already was. *)
              let stored_avatar = Option.bind loaded (fun (c : Community_types.community) -> c.avatar_url) in
              let stored_banner = Option.bind loaded (fun (c : Community_types.community) -> c.banner_url) in
              let avatar_url = if new_avatar <> None then new_avatar else stored_avatar in
              let banner_url = if new_banner <> None then new_banner else stored_banner in
              (* Network-community guard, before any write. The description is
                 canonical community identity, so a setup draft is refused
                 outright (its identity belongs to /c/:slug/setup) and a
                 published network community's description must satisfy the
                 frozen canonical policy rather than reach the scoped database
                 CHECK. A legacy community keeps exactly its previous
                 behaviour, including the untouched no-op path when the id
                 matches nothing. *)
              (match loaded with
               | Some target when is_network_setup_draft target ->
                   (* The generic community 404: nothing about the draft's
                      lifecycle, identity, or authorization is disclosed, and
                      nothing is written. *)
                   Community_read_gate.community_not_found ?user:(Dream.session_field request "username") request
               | loaded ->
                 let network_description =
                   match loaded with
                   | Some target when target.is_network_community ->
                       canonical_network_description target
                         ~raw_description:(get_field "description")
                   | _ -> Ok description
                 in
                 match network_description with
                 | Error () ->
                     (* Fail closed: no write, and no constraint name, SQL, or
                        submitted value in the response. *)
                     Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:settings_url request)
                 | Ok description ->
              (match%lwt Community_store.update_community_details db community_id description rules avatar_url banner_url with
              | Ok (Some community) ->
                  (* UPDATE ... RETURNING supplied the authoritative updated
                     record — refresh the group profile (§5.3), attributed to
                     the acting moderator/admin. *)
                  record (fun () ->
                      Analytics.identify_community_if_consented request
                        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                        (Analytics_handlers.community_group_of community));
                  Dream.redirect request settings_url
              | Ok None ->
                  (* No community matched the id: previously a silent no-op
                     UPDATE with the same redirect; keep the response, emit
                     nothing. *)
                  Dream.redirect request settings_url
              | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:settings_url request)))
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* Top Mod only — flips allow_downvotes; guards via get_moderator_role to prevent
   plain mods or non-members from toggling a community-wide setting. *)
let toggle_downvotes_handler request =
  let slug = Dream.param request "slug" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let new_val = List.assoc_opt "allow_downvotes" form_data = Some "true" in
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found "Community not found."
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
            | Ok (Some community) ->
                (* Only the admin disjunct changes: CURRENT durable authority,
                   and a generic failure rather than an override when it cannot
                   be established. The top-mod arm is untouched. *)
                (match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                | Ok is_admin ->
                let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                if not (is_top_mod || is_admin) then
                  Dream.respond ~status:`Forbidden "Only the Top Moderator can change this setting."
                else
                  match%lwt Community_store.toggle_community_downvotes db community.id new_val with
                  | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=moderation")
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err))
          )
      | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission."

(* Slice E: visibility + indexability writes from /c/:slug/settings.
   Authorization is STRICTER than the settings page itself: visibility and discovery control who
   can read a private community, so only Top Mods and global admins (TM/A) may change them — a
   regular mod hitting these POSTs directly is rejected `Forbidden, mirroring toggle_downvotes
   (top_mod || is_admin). These never change read gates or noindex behavior; they only flip the
   Slice B columns the gate/resolver already consult. *)

(* Extracted so the settings flow's lifecycle decision is unit-testable without
   Dream/DB plumbing: the handler consults exactly this, on the authoritative
   record it just loaded, before any write. The rule itself lives in
   Network_communities — this only maps the record's fields onto it and picks
   the user-facing rejection copy. *)
let visibility_update_rejection (community : Community_types.community) ~requested_visibility =
  if
    Network_communities.visibility_change_allowed
      ~is_network_community:community.is_network_community
      ~onboarding_state:community.onboarding_state
      ~requested_visibility
  then None
  else
    Some
      "Published network communities must remain public. Private channels and \
       sections may still be used."

let update_community_visibility_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* Closed-variant parse: anything other than public/private is a validation error,
              not a 500. _of_string returns None for off-enum input. *)
           (match Community_types.community_visibility_of_string (Option.value ~default:"" (List.assoc_opt "visibility" form_data)) with
            | None ->
                Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Invalid setting" ~message:"Visibility must be either public or private." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
            | Some visibility ->
                Analytics_handlers.with_analytics_after_sql (fun record ->
                Dream.sql request (fun db ->
                  match%lwt Community_store.get_community_by_slug db slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
                  | Ok (Some community) when is_network_setup_draft community ->
                      (* A setup draft's visibility is not an independent
                         switch: publication sets visibility, indexability,
                         discoverability, and onboarding state together. This
                         route must never publish one, and it never reveals
                         that the community exists in that state — the same
                         generic 404 a missing community gets, before any
                         authorization branch or write. *)
                      Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some community) ->
                      (* Only the admin disjunct changes: CURRENT durable
                         authority, and a generic failure — never an override —
                         when it cannot be established. *)
                      (match%lwt Admin_authority.current_admin_bool db request with
                      | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                      | Ok is_admin ->
                      let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                      let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                      if not (is_top_mod || is_admin) then
                        Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change visibility." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                      else
                        match visibility_update_rejection community ~requested_visibility:visibility with
                        | Some message ->
                            (* Server-side lifecycle gate: a forged POST must not
                               reach the update. `Conflict, not a redirect — the
                               transition is refused, never silently dropped. *)
                            Dream.respond ~status:`Conflict (Site_pages.msg_page ?user ~title:"Visibility unavailable" ~message ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                        | None ->
                        match%lwt Posthog_group_cleanup_job_store.update_visibility_and_enqueue db community.id visibility with
                        | Ok (Some updated, cleanup_job) ->
                            (* Visibility is a closed group property: refresh
                               the group profile from the UPDATE ... RETURNING
                               record so PostHog never holds a stale value
                               after a successful change (§5.3). On a
                               ->private transition the SAME committed
                               transaction also enqueued the durable §13
                               scrub of the previously sent name/slug; its
                               immediate attempt runs async off the response
                               path and — like §3.3 person deletion — is a
                               privacy duty, not collection, so it is not
                               consent-gated. A PostHog failure only leaves
                               the job pending; the visibility change itself
                               is already committed. *)
                            record (fun () ->
                                Analytics.identify_community_if_consented request
                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                  (Analytics_handlers.community_group_of updated);
                                match cleanup_job with
                                | Some job_id ->
                                    Lwt.async (fun () ->
                                        Analytics_handlers.attempt_posthog_group_cleanup_job
                                          request ~job_id)
                                | None -> ());
                            Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                        | Ok (None, _) ->
                            (* Community vanished between fetch and update: the
                               old code's silent no-op — same redirect, no
                               emission. *)
                            Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)))))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let update_community_indexability_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* Hidden input carries the explicit next state ("true"/"false"); a missing/garbage
              value is treated as false (non-indexable) — fail toward LESS exposure, never a 500. *)
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Community_store.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
             | Ok (Some community) when community.is_network_community ->
                 (* Indexing on a network community is never independent: a
                    setup draft must stay non-indexable, and a published one
                    is either indexable *and* discoverable or neither. This
                    route can only move [indexable], so on a network community
                    it can only produce a state
                    Network_communities.lifecycle_state_valid rejects. It
                    therefore refuses both lifecycle states outright, before
                    any authorization branch or write, with the same generic
                    404 a missing community gets — publication owns this
                    pair. *)
                 Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Ok (Some community) ->
                 (* Only the admin disjunct changes: CURRENT durable authority,
                    and a generic failure — never an override — when it cannot
                    be established. *)
                 match%lwt Admin_authority.current_admin_bool db request with
                 | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                 | Ok is_admin ->
                 let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   match%lwt Community_store.update_community_indexable db community.id indexable with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=visibility")
                   | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

(* Slice H: per-channel / per-section indexability toggles from /c/:slug/settings. Same TM/A gate
   as the Slice E community visibility/indexability controls — a regular mod hitting these POSTs
   directly is rejected `Forbidden. These flip ONLY the Slice-B child indexable columns the Slice-G
   resolver already consults for child noindex + discovery/provenance exclusion; they change no read
   gate and create no privacy. Ownership is validated (get_*_by_id) AND the UPDATE is community-scoped,
   so a forged cross-community id can neither resolve nor write. The hidden `indexable` field carries
   the explicit next state; a missing/garbage value fails toward false (non-indexable / less exposure),
   mirroring update_community_indexability_handler. *)
let update_channel_indexability_handler request =
  let slug = Dream.param request "slug" in
  let channel_id_str = Dream.param request "channel_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let channel_id = try int_of_string channel_id_str with _ -> 0 in
      if channel_id = 0 then
        Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid channel ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Community_store.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
             | Ok (Some community) ->
                 (* Only the admin disjunct changes: CURRENT durable authority,
                    and a generic failure — never an override — when it cannot
                    be established. *)
                 match%lwt Admin_authority.current_admin_bool db request with
                 | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                 | Ok is_admin ->
                 let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (match%lwt Channel_store.get_channel_by_id db channel_id community.id with
                    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
                    | Ok (Some _) ->
                        match%lwt Channel_store.update_channel_indexable db channel_id community.id indexable with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let update_section_indexability_handler request =
  let slug = Dream.param request "slug" in
  let section_id_str = Dream.param request "section_id" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let section_id = try int_of_string section_id_str with _ -> 0 in
      if section_id = 0 then
        Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Bad Request" ~message:"Invalid section ID." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
      else
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let indexable = List.assoc_opt "indexable" form_data = Some "true" in
           Dream.sql request (fun db ->
             match%lwt Community_store.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
             | Ok (Some community) ->
                 (* Only the admin disjunct changes: CURRENT durable authority,
                    and a generic failure — never an override — when it cannot
                    be established. *)
                 match%lwt Admin_authority.current_admin_bool db request with
                 | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                 | Ok is_admin ->
                 let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can change discovery settings." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (match%lwt Section_store.get_section_by_id db section_id community.id with
                    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Section not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
                    | Ok (Some _) ->
                        match%lwt Section_store.update_section_indexable db section_id community.id indexable with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")
