(* Mirrors mod_delete_post_handler exactly. slug + comment_id come from URL params;
   we must query the community to resolve community.id for the mod_actions log. *)
let mod_delete_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let slug = Dream.param request "slug" in
      let comment_id = try int_of_string (Dream.param request "id") with _ -> 0 in
      if comment_id = 0 then
        Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Bad Request" ~message:"Invalid comment ID." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
      else
      match%lwt Dream.form request with
      | `Ok form_data ->
          let reason = String.trim (List.assoc_opt "reason" form_data |> Option.value ~default:"") in
          if reason = "" then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"A reason is required for moderation actions." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
          else
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db slug with
            | Error err ->
                Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
            | Ok None ->
                Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"Community not found." ~alert_type:"error" ~return_url:"/" request)
            | Ok (Some community) ->
                (* Always query is_moderator even for admins — we need the distinction
                   to write the correct action_type in the audit log (admin_delete_comment
                   vs delete_comment), preventing admin spoofing via the community mod log.
                   is_admin is CURRENT durable authority: a stale demoted admin
                   is not a moderator here either, and an unanswerable lookup
                   removes nothing and logs nothing. *)
                match%lwt Admin_authority.current_admin_bool db request with
                | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                | Ok is_admin ->
                let%lwt is_community_mod_res = Moderator_store.is_moderator db user_id community.id in
                let is_community_mod = match is_community_mod_res with Ok true -> true | _ -> false in
                if not (is_admin || is_community_mod) then
                    Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Forbidden" ~message:"You are not a moderator of this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                else
                    (* Ownership gate: resolve comment -> post -> community BEFORE the
                       mutation. A missing comment and a comment under another
                       community's post get the same neutral 404 — the response must
                       not reveal that the numeric id exists elsewhere.
                       get_comment_post_id collapses "no row" and DB failure into one
                       Error; both are safe to treat as not-found because nothing has
                       been mutated yet. *)
                    let not_found_here () =
                      Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This comment does not exist in this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    in
                    let%lwt in_this_community =
                      match%lwt Notification_store.get_comment_post_id db comment_id with
                      | Error _ -> Lwt.return false
                      | Ok pid ->
                          (match%lwt Post_store.get_post_by_id db pid with
                          | Ok (Some post) -> Lwt.return (post.community_id = community.id)
                          | _ -> Lwt.return false)
                    in
                    if not in_this_community then not_found_here ()
                    else
                    (* Author read while the row is intact (the tombstone keeps user_id,
                       but the notification must never depend on that detail). *)
                    let%lwt author_res = Notification_store.get_comment_owner db comment_id in
                    (* The mutation re-proves comment -> post -> community atomically;
                       RETURNING c.post_id is both the match evidence and the redirect
                       target. Ok None means the comment vanished since the check above —
                       still a neutral 404, still zero side effects. *)
                    (match%lwt Admin_store.mod_delete_comment db ~community_id:community.id comment_id with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok None -> not_found_here ()
                    | Ok (Some post_id) ->
                        (* Admin acting without mod role: flag action_type and prefix reason
                           so the public mod_actions log explicitly shows "Admin Intervention". *)
                        let is_admin_override = is_admin && not is_community_mod in
                        let action_type = if is_admin_override then "admin_delete_comment" else "delete_comment" in
                        let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                        let%lwt _ = Mod_log_store.log_action db community.id user_id action_type (Some comment_id) logged_reason in
                        let%lwt _ = match author_res with
                          | Ok author_id ->
                              let msg = "Your comment was removed by a moderator. Reason: " ^ reason in
                              Notification_store.create_notif db author_id (Some post_id) "mod_action" msg
                          | Error _ -> Lwt.return (Ok ())
                        in
                        Dream.redirect request ("/p/" ^ string_of_int post_id))
          )
      | _ ->
          Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)

(* Push notifications are fan-out on write: one notification per comment, sent to
   either post owner or parent comment owner. Best-effort — failure is silently
   ignored so a notification DB error never blocks the comment submission. *)
let create_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let username = Option.value (Dream.session_field request "username") ~default:"Someone" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let content = String.trim (List.assoc_opt "content" form_data |> Option.value ~default:"") in
          let post_id_str = List.assoc_opt "post_id" form_data |> Option.value ~default:"" in
          let parent_id_opt = match List.assoc_opt "parent_id" form_data with
            | Some p when p <> "" -> (try Some (int_of_string p) with _ -> None)
            | _ -> None
          in

          if content = "" then
            Dream.html (Site_pages.msg_page ~user:username ~title:"Validation Error" ~message:"Comment cannot be empty." ~alert_type:"error" ~return_url:"/" request)
          else if String.length content > 10000 then
            Dream.html (Site_pages.msg_page ~user:username ~title:"Validation Error" ~message:"Comment cannot exceed 10,000 characters." ~alert_type:"error" ~return_url:"/" request)
          else

          let post_id = try int_of_string post_id_str with _ -> 0 in
          if post_id = 0 then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ~user:username ~title:"Form Error" ~message:"Invalid post reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Global ban gate: same reasoning as create_post_handler — active sessions
               survive a ban until the next login, so we must check on every write.
               An unreadable ban state fails closed here, before the comment
               insert, its notifications, karma and last-activity bump. *)
            match%lwt Admin_store.is_globally_banned db user_id with
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ~user:username ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
            | Ok true ->
              Dream.respond ~status:`Forbidden (Site_pages.msg_page ~user:username ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
            | Ok false ->
            (* Lookup post to get community_id for the ban check — avoids adding a hidden
               form field that a client could forge to bypass their own community ban. *)
            (match%lwt Post_store.get_post_by_id db post_id with
            | Ok (Some post) ->
                let is_tombstone = match post.content with
                  | Some "[deleted]" | Some "[removed by admin]" | Some "[removed by moderator]" -> true
                  | _ -> false
                in
                if is_tombstone then
                  Dream.respond ~status:`Forbidden "⛔ You cannot comment on a deleted post."
                else
                (* The canonical binding is unchanged: the community comes from
                   the loaded post, never from the form. Only the Error arm
                   changes — it no longer falls through to the comment insert. *)
                (match%lwt Community_ban_store.is_banned db user_id post.community_id with
                | Ok true ->
                    Dream.respond ~status:`Forbidden (Site_pages.msg_page ~user:username ~title:"Banned from Community" ~message:"You are banned from commenting in this community." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                | Error err ->
                    Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ~user:username ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                | Ok false ->
                    (* Shared Threads: the server-side participation rule.
                       One SQL capability (the same one that gates the
                       composer) requires a CURRENT path onto the canonical
                       discussion — origin membership, or membership in an
                       accepted destination whose placement is currently
                       readable, with no ban there. This closes the old
                       gap where a manual POST needed no membership at all:
                       no hidden field, route, or composer sighting grants
                       anything — every qualifying community is derived
                       from the post and its placements. The specific
                       global-ban / tombstone / origin-ban responses above
                       keep their exact observable behavior; this gate only
                       adds the membership requirement after them. Fails
                       closed on a storage error. *)
                    (match%lwt Shared_thread_reading.viewer_may_comment db ~user_id ~post_id with
                    | Error Shared_thread_reading.Storage_error ->
                        Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ~user:username ~title:"Error" ~message:"Something went wrong on our side. Please try again." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok false ->
                        Dream.respond ~status:`Forbidden (Site_pages.msg_page ~user:username ~title:"Membership required" ~message:"Only current members of a community this thread belongs to can comment." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok true ->
                    (match%lwt Comment_store.create_comment db content post_id user_id parent_id_opt with
                    | Ok `Invalid_parent ->
                        (* The submitted parent does not exist, or belongs to a
                           different post — and therefore possibly a different
                           community, possibly a private one. The store refused
                           inside the INSERT, so there is no comment row, no
                           notification, no karma change, no comment counter
                           and no activity bump to undo. The response is a
                           generic client error: it names no post, comment or
                           community, so it cannot be used to probe which
                           parent ids exist or where they live. *)
                        Dream.respond ~status:`Bad_Request (Site_pages.msg_page ~user:username ~title:"Form Error" ~message:"Invalid reply reference." ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request)
                    | Ok (`Created comment_id) ->
                        (* comment_id is the real inserted id from the step-3
                           INSERT ... RETURNING. Length/mention flags only —
                           never the comment text. *)
                        record (fun () ->
                            Analytics.capture_if_consented request
                              ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                              (Analytics.Forum_comment_created
                                 {
                                   user_id;
                                   community_id = post.community_id;
                                   post_id;
                                   comment_id;
                                   parent_comment_id = parent_id_opt;
                                   content_length = String.length content;
                                   has_mention = Handler_support.extract_mentions content <> [];
                                 }));
                        let%lwt _ = Community_user_stats_store.increment_local_comment_count db user_id post.community_id in
                        (* Bump last_activity_at so the post rises in "active" sorted feeds. *)
                        let%lwt _ = Comment_store.touch_last_activity db post_id in
                        let%lwt target_user = match parent_id_opt with
                          | Some cid -> Notification_store.get_comment_owner db cid
                          | None -> Notification_store.get_post_owner db post_id
                        in
                        let%lwt _ = match target_user with
                          | Ok target_id when target_id <> user_id ->
                              let msg = if parent_id_opt = None then username ^ " replied to your post." else username ^ " replied to your comment." in
                              Notification_store.create_notif db target_id (Some post_id) "comment_reply" msg
                          | _ -> Lwt.return (Ok ())
                        in
                        (* Fan-out @mention notifications for comment body — best-effort, skips self. *)
                        let%lwt () = Lwt_list.iter_s (fun uname ->
                          match%lwt User_store.get_user_by_username db uname with
                          | Ok (Some mentioned) when mentioned.id <> user_id ->
                              let msg = username ^ " mentioned you in a comment." in
                              let%lwt _ = Notification_store.create_notif db mentioned.id (Some post_id) "mention" msg in
                              Lwt.return_unit
                          | _ -> Lwt.return_unit
                        ) (Handler_support.extract_mentions content) in
                        (* Redirect: preserve the DESTINATION context when a
                           destination-context composer posted this comment
                           AND that context is still readable by this
                           viewer. The closed context_community field only
                           names a community — the server re-resolves the
                           placement and re-authorizes through the
                           destination's existing access rule, and the
                           Location is rebuilt from the SERVER-loaded
                           community record and canonical post, never from
                           the submitted value. Anything stale, forged, or
                           unreadable falls back to the existing canonical
                           /p/:id redirect. *)
                        let%lwt redirect_target =
                          match List.assoc_opt "context_community" form_data with
                          | Some slug when slug <> "" && slug <> post.community_slug -> (
                              match%lwt Shared_thread_reading.resolve_destination_context db ~post_id ~destination_slug:slug with
                              | Ok (Some ctx) -> (
                                  match%lwt Community_store.get_community_by_id db ctx.Shared_thread_reading.destination_community_id with
                                  | Ok (Some destination) ->
                                      let%lwt is_admin = Admin_authority.current_admin_read_override db request in
                                      let%lwt viewable = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin destination in
                                      if viewable then
                                        Lwt.return (Post_cards.canonical_thread_path destination.slug post_id post.title)
                                      else Lwt.return ("/p/" ^ string_of_int post_id)
                                  | _ -> Lwt.return ("/p/" ^ string_of_int post_id))
                              | _ -> Lwt.return ("/p/" ^ string_of_int post_id))
                          | _ -> Lwt.return ("/p/" ^ string_of_int post_id)
                        in
                        Dream.redirect request redirect_target
                    | Error err -> Dream.html (Site_pages.msg_page ~user:username ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:("/p/" ^ string_of_int post_id) request))))
            | Ok None -> Dream.html (Site_pages.msg_page ~user:username ~title:"Post Not Found" ~message:"The post you tried to comment on could not be found." ~alert_type:"error" ~return_url:"/" request)
            | Error err -> Dream.html (Site_pages.msg_page ~user:username ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request))
          ))
      | _ -> Dream.html (Site_pages.msg_page ~user:username ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

(* Authorization for the general /delete-comment endpoint. Pure and deliberately
   blind to any community id: the old handler trusted a hidden community_id form
   field for its moderator check, which let a moderator of community A delete a
   comment in community B by pairing A's id with B's comment id. Community
   moderation now lives exclusively on /c/:slug/comments/:id/mod_delete, which
   requires a reason and writes the public modlog — so this endpoint is
   author-only for non-admins, and the decision needs nothing but the session
   role and the server-resolved comment owner. *)
module Comment_delete = struct
  type decision = Admin_delete | Author_delete | Forbidden

  let decide ~is_admin ~requester_id ~owner_id =
    if is_admin then Admin_delete
    else if requester_id = owner_id then Author_delete
    else Forbidden
end

let delete_comment_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let comment_id = try int_of_string (List.assoc_opt "comment_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if comment_id = 0 then Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid comment reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Dream.sql request (fun db ->
            (* The pure decision is unchanged; only its admin input is. A stale
               demoted-admin session now decides as an ordinary user (author
               self-delete only), and an unanswerable lookup deletes nothing. *)
            match%lwt Admin_authority.current_admin_bool db request with
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok is_admin ->
            (* Resolve the target server-side: comment -> owner and parent post.
               A missing comment and a comment whose parent post is gone get the
               same neutral 404 with no mutation. *)
            let not_found () =
              Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This comment does not exist." ~alert_type:"error" ~return_url:"/" request)
            in
            let%lwt target =
              match%lwt Notification_store.get_comment_owner db comment_id with
              | Error _ -> Lwt.return None
              | Ok owner_id ->
                  (match%lwt Notification_store.get_comment_post_id db comment_id with
                  | Error _ -> Lwt.return None
                  | Ok pid ->
                      (match%lwt Post_store.get_post_by_id db pid with
                      | Ok (Some _) -> Lwt.return (Some (owner_id, pid))
                      | _ -> Lwt.return None))
            in
            match target with
            | None -> not_found ()
            | Some (owner_id, post_id) ->
                let redirect_target = "/p/" ^ string_of_int post_id in
                (match Comment_delete.decide ~is_admin ~requester_id:user_id ~owner_id with
                | Comment_delete.Forbidden ->
                    (* Moderators included: community removal must go through the
                       mod_delete flow (required reason, public modlog). *)
                    Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Forbidden" ~message:"You can only delete your own comments. Community moderation goes through the Mod Remove flow." ~alert_type:"error" ~return_url:redirect_target request)
                | Comment_delete.Admin_delete ->
                    (match%lwt Admin_store.admin_delete_comment db ~label:"[removed by admin]" comment_id with
                    | Ok () -> Dream.redirect request redirect_target
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:redirect_target request))
                | Comment_delete.Author_delete ->
                    (* The SQL is also ownership-scoped (id AND user_id), so even a
                       race with an ownership change cannot delete someone else's
                       comment. *)
                    (match%lwt Comment_store.soft_delete_comment db comment_id user_id with
                    | Ok () -> Dream.redirect request redirect_target
                    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:redirect_target request)))
          )
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)
