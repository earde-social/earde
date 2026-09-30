let add_section_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let description = (match String.trim (get "description") with "" -> None | d -> Some d) in
          let default_sort = (match get "default_sort" with "new" | "top" | "active" as s -> s | _ -> "hot") in
          let position = (try int_of_string (get "position") with _ -> 1) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Section name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (match%lwt Section_store.create_section db community.id name description position default_sort false with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let update_section_handler request =
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
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let description = (match String.trim (get "description") with "" -> None | d -> Some d) in
          let default_sort = (match get "default_sort" with "new" | "top" | "active" as s -> s | _ -> "hot") in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Section name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate section belongs to this community before updating *)
                  (match%lwt Section_store.get_section_by_id db section_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Section not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Section_store.update_section db section_id name description default_sort with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let delete_section_handler request =
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
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (match%lwt Section_store.delete_section db section_id community.id with
                   | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

(* === Live chat channel management ===
   Same security template as the section handlers above: login → form →
   community lookup → (is_admin || is_moderator) gate → ownership-validate →
   act → redirect to the settings hub. Channels are never hard-deleted; archive
   is the soft, reversible off-switch. Slug is auto-derived on create and never
   mutated on update, so existing /c/:slug/ch/:channel_slug links keep resolving
   and chat messages (keyed by channel_id) are unaffected. *)

let add_channel_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let topic = (match String.trim (get "topic") with "" -> None | t -> Some t) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Channel name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* New channel goes after the existing ones; create_channel slugifies + dedupes. *)
                  let%lwt position = match%lwt Channel_store.get_channels_by_community db community.id with
                    | Ok cs -> Lwt.return (List.length cs) | Error _ -> Lwt.return 0 in
                  (match%lwt Channel_store.create_channel db community.id name topic position with
                   | Ok _slug -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let update_channel_handler request =
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
      match%lwt Dream.form request with
      | `Ok form_data ->
          let get = fun name -> Option.value ~default:"" (List.assoc_opt name form_data) in
          let name = String.trim (get "name") in
          let topic = (match String.trim (get "topic") with "" -> None | t -> Some t) in
          if name = "" then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Validation Error" ~message:"Channel name cannot be empty." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
          else
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate the channel belongs to this community before updating. Slug is left
                     untouched — only display name + topic change. *)
                  (match%lwt Channel_store.get_channel_by_id db channel_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Channel_store.update_channel db channel_id community.id name topic with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let archive_channel_handler request =
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
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Load all channels once: validates ownership AND lets us guard the two
                     invariants — the default `general` channel and the last active channel
                     must never be archived (a community always keeps somewhere to chat). *)
                  (match%lwt Channel_store.get_channels_by_community db community.id with
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok channels ->
                       (match List.find_opt (fun (c : Channel_store.channel) -> c.id = channel_id) channels with
                        | None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                        | Some channel ->
                            let active_count = List.length (List.filter (fun (c : Channel_store.channel) -> not c.is_archived) channels) in
                            if channel.slug = "general" then
                              Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Not Allowed" ~message:"The default #general channel cannot be archived." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                            else if (not channel.is_archived) && active_count <= 1 then
                              Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot archive the last active channel — a community needs at least one." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                            else
                              (match%lwt Channel_store.set_channel_archived db channel_id community.id true with
                               | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                               | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)))))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)

let unarchive_channel_handler request =
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
      match%lwt Dream.form request with
      | `Ok _ ->
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* The admin disjunct is CURRENT durable authority; an
                   unanswerable lookup is the generic failure, never a
                   silently granted override. The moderator arm is untouched. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                | Ok is_admin ->
                let%lwt is_auth = if is_admin then Lwt.return true
                  else (match%lwt Moderator_store.is_moderator db user_id community.id with Ok b -> Lwt.return b | _ -> Lwt.return false) in
                if not is_auth then
                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Access Denied" ~message:"Moderators only." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                else
                  (* Validate ownership before flipping the flag. Unarchive is always safe. *)
                  (match%lwt Channel_store.get_channel_by_id db channel_id community.id with
                   | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Channel not found." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                   | Ok (Some _) ->
                       (match%lwt Channel_store.set_channel_archived db channel_id community.id false with
                        | Ok () -> Dream.redirect request ("/c/" ^ slug ^ "/settings?panel=channels")
                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request))))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug ^ "/settings") request)
