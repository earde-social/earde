let join_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid ->
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let redirect_url = List.assoc_opt "redirect_to" form_data |> Option.value ~default:"/" in

          if community_id = 0 then Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Slice C: no self-serve join for private communities — they are hidden and
               members are added by a mod/admin (later slice), not via this open endpoint.
               Resolve the community SERVER-SIDE (the form's community_id is untrusted) and deny
               private with the same 404 as a missing community. Public join is unchanged. *)
            match%lwt Community_store.get_community_by_id db community_id with
            | Ok (Some community) when community.Community_types.visibility = Community_types.Community_private ->
                Community_read_gate.community_not_found ?user:(Dream.session_field request "username") request
            | Ok None -> Community_read_gate.community_not_found ?user:(Dream.session_field request "username") request
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to join community. Please try again." ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (match%lwt Membership_store.join_community db (int_of_string uid) community_id with
                 | Ok () ->
                     let user_id = int_of_string uid in
                     let distinct_id = Analytics.distinct_id_of_user_id user_id in
                     record (fun () ->
                         Analytics.capture_if_consented request ~distinct_id
                           (Analytics.Community_joined
                              {
                                user_id;
                                community_id = community.id;
                                community_slug =
                                  Analytics_handlers.analytics_public_string community
                                    community.slug;
                                community_visibility =
                                  Community_types.community_visibility_to_string
                                    community.Community_types.visibility;
                              });
                         (* Full authoritative record in scope after a
                            successful join ⇒ also refresh the community group
                            profile (§5.3). *)
                         Analytics.identify_community_if_consented request
                           ~distinct_id (Analytics_handlers.community_group_of community));
                     Dream.redirect request (Handler_support.safe_local_redirect request redirect_url)
                 | Error _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Failed to join community. Please try again." ~alert_type:"error" ~return_url:"/" request))
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

let leave_community_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_id = try int_of_string (List.assoc_opt "community_id" form_data |> Option.value ~default:"") with _ -> 0 in
          let redirect_to = match List.assoc_opt "redirect_to" form_data with Some r -> r | None -> "/" in
          if community_id = 0 then Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community reference." ~alert_type:"error" ~return_url:"/" request)
          else
          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Membership_store.leave_community db user_id community_id with
            | Ok deleted ->
                (* Only when a membership row was actually deleted — a
                   non-member "leave" keeps the same redirect but is a no-op,
                   not an event. Only the form's community id is in scope; the
                   event still carries the group key through $groups (built
                   from the id), and no lookup is added just for analytics. *)
                if deleted then
                  record (fun () ->
                      Analytics.capture_if_consented request
                        ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                        (Analytics.Community_left { user_id; community_id }));
                Dream.redirect request (Handler_support.safe_local_redirect request redirect_to)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* Slice F: minimal member-management (the allow-list for private communities). Same TM/A gate as
   the Slice E visibility/indexability controls — membership controls who can read a private
   community, so a regular mod must not add/remove members. The community is always resolved from
   the slug; the form's user_id (remove) only selects WHICH row, and the DELETE is scoped to this
   community server-side, so a forged community_id is impossible. These touch ONLY community_members
   — moderator/admin rows are never affected, so removing a member who is also a mod leaves their
   role-based read access intact (correct per the product spec). Add/remove are idempotent at the
   DB layer (ON CONFLICT DO NOTHING / DELETE-no-row), so duplicates and missing rows never 500. *)
let add_member_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           let target_username = String.trim (Option.value ~default:"" (List.assoc_opt "username" form_data)) in
           Dream.sql request (fun db ->
             match%lwt Community_store.get_community_by_slug db slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
             | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
             | Ok (Some community) ->
                 (* Only the admin disjunct changes: CURRENT durable authority,
                    and a generic failure — never an override — when it cannot
                    be established. Membership controls who can read a private
                    community, so this gate must not be reachable on a stale
                    claim. *)
                 match%lwt Admin_authority.current_admin_bool db request with
                 | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                 | Ok is_admin ->
                 let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                 let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                 if not (is_top_mod || is_admin) then
                   Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can manage members." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else if target_username = "" then
                   Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Enter a username to add." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                 else
                   (* Add only EXISTING users — never create. Unknown username is a friendly 404
                      message, not a 500. *)
                   (match%lwt User_store.get_user_by_username db target_username with
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
                    | Ok None ->
                        Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"User Not Found" ~message:(Printf.sprintf "No user named \"%s\" exists." target_username) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                    | Ok (Some target) ->
                        (* Idempotent: re-adding an existing member is a no-op (ON CONFLICT DO NOTHING). *)
                        match%lwt Membership_store.join_community db target.id community.id with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=members")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")

let remove_member_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (* The member to remove is identified by user_id from the server-rendered member list.
              An absent/garbage id is a friendly validation error, never a 500. *)
           (match int_of_string_opt (String.trim (Option.value ~default:"" (List.assoc_opt "target_user_id" form_data))) with
            | None ->
                Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Invalid member selection." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
            | Some target_user_id ->
                Dream.sql request (fun db ->
                  match%lwt Community_store.get_community_by_slug db slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
                  | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)
                  | Ok (Some community) ->
                      (* Only the admin disjunct changes: CURRENT durable
                         authority, and a generic failure — never an override —
                         when it cannot be established. *)
                      match%lwt Admin_authority.current_admin_bool db request with
                      | Error e -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message e)
                      | Ok is_admin ->
                      let%lwt role_res = Moderator_store.get_moderator_role db user_id community.id in
                      let is_top_mod = match role_res with Ok (Some "top_mod") -> true | _ -> false in
                      if not (is_top_mod || is_admin) then
                        Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Only Top Mods and admins can manage members." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                      else
                        (* Idempotent: removing a non-member deletes zero rows (no error). Only the
                           community_members row is touched — moderator/admin rows are untouched.
                           No community_left analytics here: this is a moderator acting on ANOTHER
                           user's membership, not that user leaving. *)
                        match%lwt Membership_store.leave_community db target_user_id community.id with
                        | Ok _deleted -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=members")
                        | Error err -> Dream.respond ~status:`Internal_Server_Error (Handler_support.db_error_message err)))
       | _ -> Dream.respond ~status:`Bad_Request "Invalid form submission.")
