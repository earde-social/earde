let view_profile_handler request =
  let username_param = Dream.param request "username" in
  let current_user = Dream.session_field request "username" in
  let viewer_id =
    match Dream.session_field request "user_id" with
    | Some s -> ( try int_of_string s with _ -> 0)
    | None -> 0
  in
  let active_tab = Option.value ~default:"posts" (Dream.query request "tab") in

  Dream.sql request (fun db ->
      (* CURRENT durable admin authority, resolved once for the whole page: the
       private-activity filter below runs per distinct community, and must not
       become one admin lookup per community. *)
      let%lwt is_admin =
        Admin_authority.current_admin_read_override db request
      in
      let%lwt user_votes = Handler_support.get_current_user_votes db request in
      (* Joined communities feed the launch rail only (viewer's own memberships,
       same source/order as every other launch surface); a failure degrades to
       an empty rail rather than blocking the profile. *)
      let%lwt rail_communities =
        if viewer_id > 0 then
          match%lwt Membership_store.get_user_communities db viewer_id with
          | Ok cs -> Lwt.return cs
          | Error _ -> Lwt.return []
        else Lwt.return []
      in
      (* Profile leak-filter. A profile aggregates a user's activity across communities
       and is itself a PUBLIC discovery surface, so it must not surface activity that is either
       (a) PRIVATE and unreadable by the *viewer*, or (b) public-but-non-indexable, i.e.
       "unlisted". [blocked_post_ids] classifies a page of post ids in one bounded query
       (visibility + indexable), drops public-non-indexable outright, and for the rare private ones
       checks the viewer's membership/mod per distinct community. Private communities the viewer CAN
       read stay visible. Fails CLOSED: a classification error blocks the
       whole page. Public + indexable activity is unaffected. *)
      let blocked_post_ids post_ids =
        match post_ids with
        | [] -> Lwt.return []
        | _ -> (
            match%lwt Post_store.get_post_communities db post_ids with
            | Error _ -> Lwt.return post_ids
            | Ok rows ->
                (* (b) public + non-indexable → never surfaced as public discovery. *)
                let unindexed_public =
                  List.filter_map
                    (fun (pid, _cid, vis, indexable, _sec) ->
                      if vis = "public" && not indexable then Some pid else None)
                    rows
                in
                (* (c) In a non-indexable forum section → never surfaced as public discovery,
                 regardless of community-level flags. *)
                let section_excluded =
                  List.filter_map
                    (fun (pid, _cid, _vis, _ix, sec_excluded) ->
                      if sec_excluded then Some pid else None)
                    rows
                in
                (* (a) private → surfaced only to viewers who can read the community. *)
                let private_rows =
                  List.filter
                    (fun (_pid, _cid, vis, _ix, _sec) -> vis = "private")
                    rows
                in
                let distinct_cids =
                  List.sort_uniq compare
                    (List.map
                       (fun (_pid, cid, _vis, _ix, _sec) -> cid)
                       private_rows)
                in
                let%lwt readable_cids =
                  Lwt_list.filter_s
                    (fun cid ->
                      if is_admin then Lwt.return true
                      else if viewer_id <= 0 then Lwt.return false
                      else
                        let%lwt m =
                          match%lwt
                            Membership_store.is_member db viewer_id cid
                          with
                          | Ok b -> Lwt.return b
                          | Error _ -> Lwt.return false
                        in
                        if m then Lwt.return true
                        else
                          match%lwt
                            Moderator_store.is_moderator db viewer_id cid
                          with
                          | Ok b -> Lwt.return b
                          | Error _ -> Lwt.return false)
                    distinct_cids
                in
                let blocked_private =
                  List.filter_map
                    (fun (pid, cid, _vis, _ix, _sec) ->
                      if List.mem cid readable_cids then None else Some pid)
                    private_rows
                in
                Lwt.return
                  (unindexed_public @ section_excluded @ blocked_private))
      in
      (* Shared profile-surfaceable rule for community badges/stats (each carries a community record
       or slug → a discovery link): show only public+indexable communities, plus private ones the
       viewer is authorized to read. Public-but-non-indexable communities are hidden.
       Fails closed. *)
      let community_surfaceable (c : Community_types.community) =
        match c.Community_types.visibility with
        | Community_types.Community_public -> Lwt.return c.indexable
        | Community_types.Community_private ->
            Community_read_gate.can_view_community db ~user_id:viewer_id
              ~admin_override:is_admin c
      in
      let stat_is_readable (s : Community_user_stats_store.community_user_stat)
          =
        match%lwt Community_store.get_community_by_slug db s.community_slug with
        | Ok (Some community) -> community_surfaceable community
        | _ -> Lwt.return false
      in
      match%lwt User_store.get_user_public db username_param with
      | Ok (Some (uid, _, joined_at, bio, avatar_url)) -> (
          match%lwt User_store.get_user_karma db uid with
          | Ok karma -> (
              let%lwt admin_usernames_res = User_store.get_admin_usernames db in
              let admin_usernames =
                match admin_usernames_res with Ok l -> l | Error _ -> []
              in
              let%lwt moderated_communities_res =
                Moderator_store.get_moderated_communities db uid
              in
              let moderated_communities =
                match moderated_communities_res with Ok l -> l | Error _ -> []
              in
              (* The "Mod of /c/x" badges render on every tab and are discovery links.
               They would otherwise leak that this user moderates a PRIVATE community or
               an unlisted public-non-indexable one. Surface only public+indexable
               communities, plus private ones the viewer can read. *)
              let%lwt moderated_communities =
                Lwt_list.filter_s community_surfaceable moderated_communities
              in
              (* Presentation only: the profile SUBJECT's ban badge and which of
               the admin Ban/Unban forms is drawn. It authorizes nothing — both
               forms re-check admin server-side — so degrading to false on a
               read failure is a display fallback, not a permission decision. *)
              let%lwt is_gb_res = Admin_store.is_globally_banned db uid in
              let is_globally_banned =
                match is_gb_res with Ok b -> b | Error _ -> false
              in

              (* Fetch only the data the active tab needs — avoids double DB round-trips. *)
              if active_tab = "comments" then
                match%lwt Comment_store.get_comments_by_user db uid with
                | Ok user_comments ->
                    let post_ids =
                      List.map (fun (_, _, _, pid, _, _) -> pid) user_comments
                    in
                    let%lwt blocked = blocked_post_ids post_ids in
                    let user_comments =
                      List.filter
                        (fun (_, _, _, pid, _, _) -> not (List.mem pid blocked))
                        user_comments
                    in
                    Dream.html
                      (Account_pages.user_profile_page ?user:current_user
                         ~is_admin ~is_globally_banned ~profile_id:uid
                         ~admin_usernames ~moderated_communities ~active_tab
                         ~rail_communities user_votes username_param joined_at
                         bio avatar_url karma [] user_comments [] request)
                | Error err ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user:current_user ~title:"Error"
                         ~message:(Handler_support.db_error_message err)
                         ~alert_type:"error" ~return_url:"/" request)
              else if active_tab = "communities" then
                match%lwt
                  Community_user_stats_store.get_user_community_stats db uid
                with
                | Ok community_stats ->
                    let%lwt community_stats =
                      Lwt_list.filter_s stat_is_readable community_stats
                    in
                    Dream.html
                      (Account_pages.user_profile_page ?user:current_user
                         ~is_admin ~is_globally_banned ~profile_id:uid
                         ~admin_usernames ~moderated_communities ~active_tab
                         ~rail_communities user_votes username_param joined_at
                         bio avatar_url karma [] [] community_stats request)
                | Error err ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user:current_user ~title:"Error"
                         ~message:(Handler_support.db_error_message err)
                         ~alert_type:"error" ~return_url:"/" request)
              else
                match%lwt Post_store.get_posts_by_user db uid with
                | Ok posts ->
                    let post_ids =
                      List.map (fun (p : Post_types.post) -> p.id) posts
                    in
                    let%lwt blocked = blocked_post_ids post_ids in
                    let posts =
                      List.filter
                        (fun (p : Post_types.post) ->
                          not (List.mem p.id blocked))
                        posts
                    in
                    Dream.html
                      (Account_pages.user_profile_page ?user:current_user
                         ~is_admin ~is_globally_banned ~profile_id:uid
                         ~admin_usernames ~moderated_communities ~active_tab
                         ~rail_communities user_votes username_param joined_at
                         bio avatar_url karma posts [] [] request)
                | Error err ->
                    Dream.respond ~status:`Internal_Server_Error
                      (Site_pages.msg_page ?user:current_user ~title:"Error"
                         ~message:(Handler_support.db_error_message err)
                         ~alert_type:"error" ~return_url:"/" request))
          | Error err ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page ?user:current_user ~title:"Error"
                   ~message:(Handler_support.db_error_message err)
                   ~alert_type:"error" ~return_url:"/" request))
      | Ok None ->
          Dream.respond ~status:`Not_Found
            (Site_pages.msg_page ?user:current_user ~title:"Not Found"
               ~message:"This user does not exist." ~alert_type:"error"
               ~return_url:"/" request)
      | Error err ->
          Dream.respond ~status:`Internal_Server_Error
            (Site_pages.msg_page ?user:current_user ~title:"Error"
               ~message:(Handler_support.db_error_message err)
               ~alert_type:"error" ~return_url:"/" request))

let settings_page_handler request =
  match Dream.session_field request "username" with
  | None -> Dream.redirect request "/login"
  | Some username ->
      Dream.sql request (fun db ->
          match%lwt User_store.get_user_public db username with
          | Ok (Some (_, _, _, bio, avatar_url)) ->
              (* Joined communities feed the launch rail only; a failure (or a
               missing/garbled user_id session field) degrades to an empty
               rail rather than blocking the settings page. *)
              let%lwt rail_communities =
                match
                  Option.bind
                    (Dream.session_field request "user_id")
                    int_of_string_opt
                with
                | Some uid -> (
                    match%lwt Membership_store.get_user_communities db uid with
                    | Ok cs -> Lwt.return cs
                    | Error _ -> Lwt.return [])
                | None -> Lwt.return []
              in
              Dream.html
                (Account_pages.settings_page ~user:username ~rail_communities
                   bio avatar_url request)
          | _ -> Dream.redirect request "/login")

let update_profile_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let bio = match get_field "bio" with "" -> None | b -> Some b in
          let avatar_bytes = get_field "avatar_url" in
          (* The browser supplies bytes for a NEW avatar and nothing else.
             The form used to also round-trip the stored URL in a hidden
             existing_avatar_url field, which this handler wrote back
             verbatim — so a caller could set their own users.avatar_url to
             ANY string, including another user's /static/uploads/ file, and
             then have delete_account_handler unlink it. The submitted value
             is now ignored entirely (the field is gone from the form) and
             the fallback is re-read from the caller's own row, which is the
             only avatar they could legitimately keep. *)
          Dream.sql request (fun db ->
              match%lwt
                Image_processing.process_image_upload ~db
                  ~ip:(Dream.client request)
                  ~purpose:Image_upload.Profile_avatar avatar_bytes
              with
              | Error e ->
                  Dream.html
                    (Site_pages.msg_page
                       ?user:(Dream.session_field request "username")
                       ~title:"Image Error" ~message:e ~alert_type:"error"
                       ~return_url:"/settings" request)
              | Ok new_avatar -> (
                  (* A failed read must not silently clear the stored avatar: the
               profile write is refused instead. *)
                  let%lwt stored_avatar =
                    match new_avatar with
                    | Some _ -> Lwt.return (Ok new_avatar)
                    | None -> User_store.get_user_avatar_url db user_id
                  in
                  match stored_avatar with
                  | Error err ->
                      Dream.respond ~status:`Internal_Server_Error
                        (Site_pages.msg_page
                           ?user:(Dream.session_field request "username")
                           ~title:"Error"
                           ~message:(Handler_support.db_error_message err)
                           ~alert_type:"error" ~return_url:"/settings" request)
                  | Ok avatar_url -> (
                      match%lwt
                        User_store.update_user_profile db bio avatar_url user_id
                      with
                      | Ok () -> Dream.redirect request "/settings"
                      | Error err ->
                          Dream.respond ~status:`Internal_Server_Error
                            (Site_pages.msg_page
                               ?user:(Dream.session_field request "username")
                               ~title:"Error"
                               ~message:(Handler_support.db_error_message err)
                               ~alert_type:"error" ~return_url:"/settings"
                               request))))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page
               ?user:(Dream.session_field request "username")
               ~title:"Form Error" ~message:"Invalid form submission."
               ~alert_type:"error" ~return_url:"/settings" request))

(* Re-authenticate with old password before rotating the secret — prevents session
   hijack from silently changing credentials via a stolen cookie. *)
let change_password_handler request =
  match
    ( Dream.session_field request "user_id",
      Dream.session_field request "username" )
  with
  | Some uid_str, Some username -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data -> (
          let old_password = List.assoc "old_password" form_data in
          let new_password = List.assoc "new_password" form_data in
          let confirm_password = List.assoc "confirm_password" form_data in

          if new_password <> confirm_password then
            Dream.html
              (Site_pages.msg_page ~user:username ~title:"Password Mismatch"
                 ~message:
                   "The new passwords you entered do not match. Please go back \
                    and try again."
                 ~alert_type:"error" ~return_url:"/settings" request)
          else if String.length new_password < 8 then
            Dream.html
              (Site_pages.msg_page ~user:username ~title:"Password Too Short"
                 ~message:
                   "Your new password must be at least 8 characters long."
                 ~alert_type:"error" ~return_url:"/settings" request)
          else
            (* Argon2 verify/hash run OUTSIDE Dream.sql — CPU-bound work must
               not hold a pool connection (same rule as login_handler), and
               invalidate_session below needs its own connection. *)
            let%lwt lookup =
              Dream.sql request (fun db ->
                  User_store.get_user_for_login db username)
            in
            match lookup with
            | Ok (Some (_, (hash, _, _))) -> (
                match%lwt Auth.verify_password ~password:old_password ~hash with
                | Ok true -> (
                    match%lwt Auth.hash_password new_password with
                    | Ok new_hash -> (
                        let%lwt updated =
                          Dream.sql request (fun db ->
                              Credential_store.update_password_revoking_sessions
                                db user_id new_hash)
                        in
                        match updated with
                        | Ok () ->
                            (* Every server-side session of this user is gone,
                               including the one behind this request's cookie;
                               invalidate_session resets THIS request's session
                               too, so the response carries a fresh anonymous
                               cookie instead of a stale authenticated one. *)
                            let%lwt () = Dream.invalidate_session request in
                            Dream.html
                              (Site_pages.msg_page ~auth:true
                                 ~title:"Password Changed"
                                 ~message:
                                   "Your password has been updated. For \
                                    security, all of your sessions have been \
                                    signed out — please log in again with your \
                                    new password."
                                 ~alert_type:"success" ~return_url:"/login"
                                 request)
                        | Error err ->
                            Dream.html
                              (Site_pages.msg_page ~user:username ~title:"Error"
                                 ~message:(Handler_support.db_error_message err)
                                 ~alert_type:"error" ~return_url:"/settings"
                                 request))
                    | Error err ->
                        Dream.html
                          (Site_pages.msg_page ~user:username ~title:"Error"
                             ~message:("Hashing error: " ^ err)
                             ~alert_type:"error" ~return_url:"/settings" request)
                    )
                | _ ->
                    Dream.html
                      (Site_pages.msg_page ~user:username
                         ~title:"Wrong Password"
                         ~message:
                           "The current password you entered is incorrect. \
                            Please go back and try again."
                         ~alert_type:"error" ~return_url:"/settings" request))
            | _ ->
                Dream.html
                  (Site_pages.msg_page ~user:username ~title:"Error"
                     ~message:"User not found in the database."
                     ~alert_type:"error" ~return_url:"/settings" request))
      | _ ->
          Dream.html
            (Site_pages.msg_page ~user:username ~title:"Form Error"
               ~message:
                 "There was a problem with your form submission. Please try \
                  again."
               ~alert_type:"error" ~return_url:"/settings" request))
  | _ -> Dream.redirect request "/login"

(* GDPR Art. 20 (data portability): JSON chosen over CSV for machine-readability;
   Content-Disposition triggers browser download rather than inline render. *)
let export_data_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let username =
        Option.value (Dream.session_field request "username") ~default:"unknown"
      in

      Dream.sql request (fun db ->
          let%lwt profile_res = User_store.get_user_public db username in
          let%lwt posts_res = Post_store.get_posts_by_user db user_id in
          let%lwt comments_res =
            Comment_store.get_comments_by_user db user_id
          in

          match (profile_res, posts_res, comments_res) with
          | Ok (Some (_, _, joined_at, bio, avatar)), Ok posts, Ok comments ->
              let profile_json =
                `Assoc
                  [
                    ("username", `String username);
                    ("joined_at", `String joined_at);
                    ("bio", match bio with Some b -> `String b | None -> `Null);
                    ( "avatar_url",
                      match avatar with Some a -> `String a | None -> `Null );
                  ]
              in

              let posts_json =
                `List
                  (List.map
                     (fun (p : Post_types.post) ->
                       `Assoc
                         [
                           ("id", `Int p.id);
                           ("title", `String p.title);
                           ( "content",
                             match p.content with
                             | Some c -> `String c
                             | None -> `Null );
                           ( "url",
                             match p.url with
                             | Some u -> `String u
                             | None -> `Null );
                           ("community_slug", `String p.community_slug);
                           ("created_at", `String p.created_at);
                           ("score", `Int p.score);
                         ])
                     posts)
              in

              let comments_json =
                `List
                  (List.map
                     (fun (id, content, created_at, post_id, post_title, score)
                        ->
                       `Assoc
                         [
                           ("id", `Int id);
                           ("post_id", `Int post_id);
                           ("post_title", `String post_title);
                           ("content", `String content);
                           ("created_at", `String created_at);
                           ("score", `Int score);
                         ])
                     comments)
              in

              let export_json =
                `Assoc
                  [
                    ("profile", profile_json);
                    ("posts", posts_json);
                    ("comments", comments_json);
                  ]
              in

              let json_str = Yojson.Safe.pretty_to_string export_json in
              Dream.respond
                ~headers:
                  [
                    ("Content-Type", "application/json");
                    (* The filename is derived from the numeric user id, never the
                   username: a header value must stay within a conservative
                   ASCII alphabet (no quotes, control bytes, or separators). *)
                    ( "Content-Disposition",
                      Printf.sprintf
                        "attachment; filename=\"earde_export_user_%d.json\""
                        user_id );
                  ]
                json_str
          | _ ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page
                   ?user:(Dream.session_field request "username")
                   ~title:"Error"
                   ~message:"Failed to generate export data. Please try again."
                   ~alert_type:"error" ~return_url:"/settings" request))

(* Immediate §3.3 attempt for a just-committed deletion job. The atomic claim
   (status + lease in one statement) means a concurrently running maintenance
   retry can never process the same job at the same time; every DB step is its
   own short Dream.sql call, so no connection is held across the PostHog HTTP
   attempt. All failures are swallowed — the job stays durably pending. *)
let attempt_posthog_deletion_job request ~job_id =
  Lwt.catch
    (fun () ->
      let%lwt claimed =
        Dream.sql request (fun db -> Posthog_deletion_job_store.claim db job_id)
      in
      match claimed with
      | Ok (Some distinct_id) ->
          let%lwt (_ : [ `Completed | `Left_pending of string ]) =
            Posthog_deletion.process_claimed_job
              ~mark_completed:(fun () ->
                Dream.sql request (fun db ->
                    Posthog_deletion_job_store.mark_completed db job_id))
              ~mark_failed:(fun err ->
                Dream.sql request (fun db ->
                    Posthog_deletion_job_store.mark_failed db job_id err))
              ~distinct_id
          in
          Lwt.return_unit
      | Ok None | Error _ -> Lwt.return_unit)
    (fun exn ->
      Dream.log "posthog deletion immediate attempt error: %s"
        (Printexc.to_string exn);
      Lwt.return_unit)

(* GDPR Art. 17 (right to erasure): anonymize rather than hard-delete to preserve
   thread coherence; posts remain as [deleted] rather than leaving orphaned replies. *)
let delete_account_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str -> (
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok _ -> (
          (* The stored avatar reference must be read BEFORE the anonymize
             rewrite NULLs it. A read failure only skips file cleanup — it
             must never block the deletion itself. *)
          let%lwt avatar_url =
            Lwt.catch
              (fun () ->
                let%lwt res =
                  Dream.sql request (fun db ->
                      User_store.get_user_avatar_url db user_id)
                in
                match res with
                | Ok v -> Lwt.return v
                | Error _ -> Lwt.return None)
              (fun _ -> Lwt.return None)
          in
          (* §3.3 atomic local deletion: anonymization and the durable
             deletion job commit together (or roll back together) — no crash
             window with an anonymized user and no job. The transaction never
             performs HTTP. *)
          let%lwt result =
            Dream.sql request (fun db ->
                Posthog_deletion_job_store.anonymize_and_enqueue db user_id)
          in
          match result with
          | Ok (job_id, _distinct_id) ->
              (* One async cleanup chain, off the response path:
                   1. the locally stored avatar file, only after the commit
                      (Avatar_uploads validates the path shape; anything not
                      a pipeline upload is untouched, a missing file is
                      success, and a real failure logs a fixed, path-free
                      line and never unwinds the committed deletion);
                   2. the consent-gated PERSONLESS account_deleted metric
                      (constant system distinct id, person processing off — so
                      ingestion timing can never associate it with, or
                      recreate, the person being deleted);
                   3. then — regardless of the metric's outcome — the
                      immediate durable deletion attempt for the real
                      user:<id> job. PostHog being down or unconfigured only
                      leaves the committed job pending. *)
              Lwt.async (fun () ->
                  (match
                     Avatar_uploads.cleanup_deleted_account_avatar avatar_url
                   with
                  | `Removed | `Absent | `Not_local -> ()
                  | `Failed ->
                      Dream.log
                        "avatar cleanup failed for a deleted account; file \
                         retained under static/uploads");
                  let%lwt () =
                    Lwt.catch
                      (fun () ->
                        Analytics.capture_account_deleted_sequenced request)
                      (fun _ -> Lwt.return_unit)
                  in
                  attempt_posthog_deletion_job request ~job_id);
              let%lwt () = Dream.invalidate_session request in
              Dream.redirect request "/"
          | Error err ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page
                   ?user:(Dream.session_field request "username")
                   ~title:"Error"
                   ~message:(Handler_support.db_error_message err)
                   ~alert_type:"error" ~return_url:"/settings" request))
      | _ ->
          Dream.respond ~status:`Bad_Request
            (Site_pages.msg_page
               ?user:(Dream.session_field request "username")
               ~title:"Form Error" ~message:"Invalid form submission."
               ~alert_type:"error" ~return_url:"/settings" request))

(* === NOTIFICATIONS === *)

let notifications_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      (* The session claim only enables the durable users.is_admin check
         inside the capability columns, never replaces it. *)
      let session_admin =
        Dream.session_field request "is_admin" = Some "true"
      in
      Dream.sql request (fun db ->
          let%lwt notifs =
            Notification_store.get_notifications db ~session_admin user_id
          in
          let%lwt _ = Notification_store.mark_notifs_read db user_id in
          (* Joined communities feed the launch rail only; a failure degrades to
           an empty rail rather than blocking the notification list. *)
          let%lwt rail_communities_res =
            Membership_store.get_user_communities db user_id
          in
          let rail_communities =
            match rail_communities_res with Ok cs -> cs | Error _ -> []
          in
          match notifs with
          | Ok n ->
              Dream.html
                (Account_pages.notifications_page ?user ~rail_communities n
                   request)
          | Error e ->
              Dream.respond ~status:`Internal_Server_Error
                (Site_pages.msg_page ?user ~title:"Error"
                   ~message:(Handler_support.db_error_message e)
                   ~alert_type:"error" ~return_url:"/" request))
