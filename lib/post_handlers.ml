let new_post_page request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let user = Dream.session_field request "username" in
      let community_slug_opt = Dream.query request "community" in
      let section_slug_opt = Dream.query request "section" in

      (* Joined communities feed the launch rail only; a failure degrades to
         an empty rail rather than blocking the composer. Called only after
         the viewer is authorized for the requested state. *)
      let load_rail db =
        match%lwt Membership_store.get_user_communities db user_id with
        | Ok cs -> Lwt.return cs
        | Error _ -> Lwt.return []
      in

      match community_slug_opt with
      | Some slug ->
          Dream.sql request (fun db ->
            (* CURRENT durable admin authority for the private read gate. *)
            let%lwt is_admin = Admin_authority.current_admin_read_override db request in
            match%lwt Community_store.get_community_by_slug db slug with
            | Ok (Some community) ->
                (* Privacy gate: a private community must be indistinguishable
                   from a missing one for outsiders — the same rule as the
                   overview/section/thread/report surfaces. Previously this
                   route answered a non-member's ?community=<private-slug>
                   with the join gate, confirming existence and leaking the
                   community name in the title. *)
                let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
                if not authorized then Community_read_gate.community_not_found ?user request
                else
                (match%lwt Membership_store.is_member db user_id community.id with
                | Ok true ->
                    let%lwt sections =
                      if community.sections_enabled then
                        (match%lwt Section_store.get_sections_by_community db community.id with
                         | Ok secs -> Lwt.return secs
                         | Error _ -> Lwt.return [])
                      else Lwt.return []
                    in
                    (* Resolve ?section=slug to section_id for pre-selection in the form *)
                    let%lwt preselected_section_id_opt = match section_slug_opt with
                      | None -> Lwt.return None
                      | Some sec_slug ->
                          (match%lwt Section_store.get_section_by_slug db sec_slug community.id with
                           | Ok (Some s) -> Lwt.return (Some s.Section_store.section_id)
                           | _ -> Lwt.return None)
                    in
                    let%lwt rail_communities = load_rail db in
                    (* Optional shared-thread destinations (slice 4): the
                       eligible connected communities for THIS server-resolved
                       origin. Best-effort like the rail — a read failure
                       renders the plain composer rather than blocking post
                       creation; the list grants nothing (POST /posts
                       re-resolves the slug and the placement store
                       revalidates under its own locks). *)
                    let%lwt share_candidates =
                      match%lwt
                        Shared_thread_placement_read_model.connected_destinations
                          db ~origin_community_id:community.id
                      with
                      | Ok cs ->
                          Lwt.return
                            (List.map
                               (fun c ->
                                 ( Shared_thread_placement_read_model.candidate_slug c,
                                   Shared_thread_placement_read_model.candidate_name c ))
                               cs)
                      | Error _ -> Lwt.return []
                    in
                    Dream.html (Post_pages.new_post_form ?user ?preselected_section_id:preselected_section_id_opt ~rail_communities ~share_candidates sections community request)
                | Ok false ->
                    let%lwt rail_communities = load_rail db in
                    Dream.html (Post_pages.join_to_post_page ?user ~rail_communities community request)
                | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request))

            | Ok None -> Community_read_gate.community_not_found ?user request
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )
      | None ->
          Dream.sql request (fun db ->
            (* One durable resolution for the whole chooser: the per-community
               filter below must not turn into one admin lookup per row. *)
            let%lwt is_admin = Admin_authority.current_admin_read_override db request in
            match%lwt Community_store.get_all_communities db with
            | Ok communities ->
                (* The chooser must never list a community the viewer cannot
                   see: get_all_communities returns every row, and the legacy
                   page exposed private community names and slugs to any
                   logged-in user. Filter with the same per-community
                   authorization the content surfaces use. *)
                let%lwt visible =
                  Lwt_list.filter_s
                    (fun c -> Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin c)
                    communities
                in
                let%lwt rail_communities = load_rail db in
                Dream.html (Post_pages.choose_community_page ?user ~request ~rail_communities visible)
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )

let create_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some user_id_str ->
      let user_id = int_of_string user_id_str in
      let username = Option.value (Dream.session_field request "username") ~default:"Someone" in

      (* multipart/form-data: required for file upload; replaces form which only handles
         application/x-www-form-urlencoded. Dream.multipart returns
         (field_name * (filename_opt * content) list) list — extract the first part value. *)
      match%lwt Dream.multipart request with
      | `Ok form_data ->
          let get_field name =
            match List.assoc_opt name form_data with
            | Some ((_, v) :: _) -> v
            | _ -> ""
          in
          let title = String.trim (get_field "title") in
          let community_id_str = get_field "community_id" in
          let url = match get_field "url" with "" -> None | u -> Some u in
          let content = match get_field "content" with "" -> None | c -> Some c in
          let section_id_str = get_field "section_id" in
          (* File bytes: empty string when no file is selected (browser sends empty part). *)
          let image_bytes = get_field "image" in
          (* Optional shared-thread fields (slice 4). A blank select is the
             normal share-free path; a selected slug is only re-resolved
             server-side AFTER the post exists, because the canonical post
             must never depend on any destination condition. The note alone
             is judged now — deterministic user-input validation through the
             one domain canonicalizer — so a hopeless note fails before a
             post exists. A note without a destination is ignored. *)
          let share_destination =
            match String.trim (get_field "share_destination") with
            | "" -> None
            | slug -> Some slug in
          let share_note_result =
            match share_destination with
            | None -> Ok None
            | Some _ ->
                Shared_thread_placements.canonical_request_note
                  (Some (get_field "share_note")) in

          if title = "" then
            Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot be empty." ~alert_type:"error" ~return_url:"/" request)
          else if String.length title > 300 then
            Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Post title cannot exceed 300 characters." ~alert_type:"error" ~return_url:"/" request)
          else if image_bytes <> "" && String.length image_bytes > 5 * 1024 * 1024 then
            Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Image exceeds the 5 MB limit." ~alert_type:"error" ~return_url:"/" request)
          else if Result.is_error share_note_result then
            Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"The private request note is too long or contains characters that cannot be stored. Notes can hold up to 2,000 characters." ~alert_type:"error" ~return_url:"/" request)
          else

          let community_id = try int_of_string community_id_str with _ -> 0 in
          if community_id = 0 then
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid community selection." ~alert_type:"error" ~return_url:"/" request)
          else

          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            (* Global ban gate: checked first — a globally banned user's session may
               still be active if they were banned after logging in. Failing to
               READ that state is a storage failure, not permission: it stops
               here, ahead of image processing and the post insert. *)
            match%lwt Admin_store.is_globally_banned db user_id with
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
            | Ok true ->
              Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request)
            | Ok false ->
              (match%lwt Membership_store.is_member db user_id community_id with
              | Ok true ->
                  (* Local ban check: evaluated only for members. *)
                  (match%lwt Community_ban_store.is_banned db user_id community_id with
                  | Ok true ->
                      Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Banned from Community" ~message:"You are banned from posting in this community." ~alert_type:"error" ~return_url:"/" request)
                  (* Fail closed BEFORE process_image_upload: an unreadable local
                     ban must not buy a full ImageMagick conversion, let alone a
                     post row. *)
                  | Error err ->
                      Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
                  | Ok false ->
                  (* Image processing runs here, after the global-ban,
                     membership and community-ban gates. It used to run before
                     all three, so a banned user or a non-member could force a
                     full ImageMagick conversion and leave a file in
                     static/uploads for any community id and still be refused
                     the post. *)
                  (match%lwt Image_processing.process_image_upload ~db ~ip:(Dream.client request)
                               ~purpose:Image_upload.Post_image image_bytes with
                  | Error img_err ->
                      Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Image Error" ~message:img_err ~alert_type:"error" ~return_url:"/" request)
                  | Ok image_url ->
                      (* Server-side section validation: section_id must belong to this community.
                         Prevents posting to a section from a different community via crafted form. *)
                      (* Loaded once: section validation here and, on the
                         sharing path, the canonical redirect target after
                         creation. The Error/Ok None outcomes keep their
                         exact pre-slice-4 behavior. *)
                      let%lwt community_record = Community_store.get_community_by_id db community_id in
                      let%lwt section_result =
                        match community_record with
                        | Error e -> Lwt.return (Error e)
                        | Ok None -> Lwt.return (Ok None)
                        | Ok (Some comm) ->
                            if not comm.sections_enabled then
                              Lwt.return (Ok None)
                            else begin
                              let sid = try int_of_string section_id_str with _ -> 0 in
                              if sid = 0 then
                                Lwt.return (Error "section_required")
                              else
                                match%lwt Section_store.get_section_by_id db sid community_id with
                                | Ok (Some _) -> Lwt.return (Ok (Some sid))
                                | Ok None    -> Lwt.return (Error "section_invalid")
                                | Error e    -> Lwt.return (Error e)
                            end
                      in
                      (match section_result with
                      | Error "section_required" ->
                          Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Please select a section for your post." ~alert_type:"error" ~return_url:"/" request)
                      | Error "section_invalid" ->
                          Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Invalid Section" ~message:"The selected section does not belong to this community." ~alert_type:"error" ~return_url:"/" request)
                      | Error e ->
                          Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                      | Ok section_id ->
                          (match%lwt Post_store.create_post db title url content image_url section_id community_id user_id with
                          | Ok new_post_id ->
                              let%lwt _ = Community_user_stats_store.increment_local_post_count db user_id community_id in
                              (* Fan-out @mention notifications — best-effort, skips self-mentions. *)
                              let text = title ^ " " ^ (Option.value ~default:"" content) in
                              (* Closed derived flags only — never the title,
                                 body, or URL themselves. *)
                              (* Named once: the sharing branch below must
                                 compose with this capture, because record
                                 holds a single pending slot and a second
                                 record call would silently replace it. *)
                              let capture_creation () =
                                Analytics.capture_if_consented request
                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                  (Analytics.Forum_thread_created
                                     {
                                       user_id;
                                       community_id;
                                       section_id;
                                       post_id = new_post_id;
                                       content_length =
                                         String.length (Option.value ~default:"" content);
                                       has_link = url <> None;
                                       has_mention = Handler_support.extract_mentions text <> [];
                                     }) in
                              record capture_creation;
                              let%lwt () = Lwt_list.iter_s (fun uname ->
                                match%lwt User_store.get_user_by_username db uname with
                                | Ok (Some mentioned) when mentioned.id <> user_id ->
                                    let msg = username ^ " mentioned you in a post." in
                                    let%lwt _ = Notification_store.create_notif db mentioned.id (Some new_post_id) "mention" msg in
                                    Lwt.return_unit
                                | _ -> Lwt.return_unit
                              ) (Handler_support.extract_mentions text) in
                              (match share_destination with
                              | None ->
                              (* Redirect to the new post rather than "/" so the author
                                 immediately sees their submission with its canonical URL. *)
                                  Dream.redirect request ("/p/" ^ string_of_int new_post_id)
                              | Some destination_slug ->
                                  (* The canonical post is committed and every normal side
                                     effect above has already run; nothing below may undo
                                     any of it. The slug is re-resolved server-side and the
                                     placement store revalidates connection, eligibility,
                                     tombstone state and uniqueness under its own locks —
                                     its transaction stays atomic (placement + audit +
                                     notifications, or nothing). Every failure — tampered
                                     or vanished destination, lost connection, eligibility
                                     drift, store error — collapses into the one fixed
                                     partial-success notice: which condition failed never
                                     surfaces, and no raw error crosses. *)
                                  let share_note =
                                    match share_note_result with Ok n -> n | Error _ -> None in
                                  let%lwt requested =
                                    match%lwt
                                      Shared_thread_placement_read_model.resolve_destination
                                        db ~slug:destination_slug
                                    with
                                    | Ok (Some destination_community_id) -> (
                                        match%lwt
                                          Shared_thread_placement_store.request db
                                            ~actor_user_id:user_id ~post_id:new_post_id
                                            ~destination_community_id
                                            ~request_note:share_note
                                        with
                                        | Ok _ ->
                                            (* Convention: captured only for the committed
                                               request; a failed attempt produces nothing.
                                               Origin id + post id only — never the
                                               destination or the private note. Composed
                                               with the creation capture: record holds one
                                               slot, and the normal creation event must
                                               keep firing unchanged. *)
                                            record (fun () ->
                                                capture_creation ();
                                                Analytics.capture_if_consented request
                                                  ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                                  (Analytics.Shared_thread_request_submitted
                                                     { user_id; community_id; post_id = new_post_id }));
                                            Lwt.return true
                                        | Error _ -> Lwt.return false)
                                    | Ok None | Error _ -> Lwt.return false
                                  in
                                  let notice = if requested then "requested" else "failed" in
                                  (* PRG onto the canonical origin thread — never a composer
                                     re-render, which would invite a duplicate submission.
                                     The path comes from the server-loaded community record;
                                     the pathological missing-record case falls back to the
                                     legacy /p/:id redirect (the notice is lost, the thread
                                     is not). *)
                                  (match community_record with
                                  | Ok (Some comm) ->
                                      Dream.redirect request
                                        (Post_cards.canonical_thread_path comm.slug new_post_id title
                                         ^ "?shared=" ^ notice)
                                  | _ -> Dream.redirect request ("/p/" ^ string_of_int new_post_id)))
                          | Error err -> Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)))))
              | Ok false ->
                  Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not a Member" ~message:"You must join this community before you can post in it." ~alert_type:"error" ~return_url:"/" request)
              | Error err -> Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request))
          ))
      | _ -> Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"There was a problem with your form submission. Please try again." ~alert_type:"error" ~return_url:"/" request)

let view_post_handler request =
  let user_sess = Dream.session_field request "username" in
  let user_id_opt = Dream.session_field request "user_id" in
  (* Guard against /p/notanumber — Dream's router only enforces :id is non-empty. *)
  let post_id_opt = try Some (int_of_string (Dream.param request "id")) with _ -> None in
  match post_id_opt with
  | None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"Invalid post ID." ~alert_type:"error" ~return_url:"/" request)
  | Some post_id ->

  Dream.sql request (fun db ->
    match%lwt Post_store.get_post_by_id db post_id with
    | Ok (Some post) ->
        (* Slice C: gate BEFORE the canonical redirect — a 301 to /c/:slug/t/:id-:title would
           otherwise leak a private community's slug + thread title in the Location header to a
           non-authorized viewer. Resolve visibility from the post's community_id (never client
           input); deny with the SAME 404 as a missing community. Fail closed if the community
           can't be resolved. *)
        let viewer_id = match user_id_opt with Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        (* CURRENT durable admin authority for the private read gate. *)
        let%lwt is_admin = Admin_authority.current_admin_read_override db request in
        let%lwt gate_ok =
          match%lwt Community_store.get_community_by_id db post.community_id with
          | Ok (Some community) -> Community_read_gate.can_view_community db ~user_id:viewer_id ~admin_override:is_admin community
          | _ -> Lwt.return false
        in
        if not gate_ok then Community_read_gate.community_not_found ?user:user_sess request
        else if post.community_slug <> "" then
          (* /p/:id is legacy: 301 to the canonical thread URL so we converge on one URL model.
             post_id stays authoritative; the canonical route renders the shell. *)
          Dream.redirect ~status:`Moved_Permanently request
            (Post_cards.canonical_thread_path post.community_slug post.id post.title)
        else begin
        (* Safe fallback for the pathological unmappable post (no community slug): render the
           legacy warm-card page rather than 500. In practice community_slug is always set. *)
        let%lwt comments_result = Comment_store.get_comments db post.id in

        let%lwt is_member_result =
          match user_id_opt with
          | Some uid -> Membership_store.is_member db (int_of_string uid) post.community_id
          | None -> Lwt.return (Ok false)
        in

        let%lwt user_post_votes = Handler_support.get_current_user_votes db request in
        let%lwt user_comment_votes = Handler_support.get_current_user_comment_votes db request in
        let%lwt is_mod_res =
          match user_id_opt with
          | Some uid -> Moderator_store.is_moderator db (int_of_string uid) post.community_id
          | None -> Lwt.return_ok false
        in
        let%lwt mods_res = Moderator_store.get_community_moderators db post.community_id in

        let%lwt admin_usernames_res = User_store.get_admin_usernames db in
        let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
        let%lwt banned_res = Community_ban_store.get_banned_users db post.community_id in
        let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: User_store.user) -> u.username) bs | _ -> [] in
        let%lwt community_res = Community_store.get_community_by_slug db post.community_slug in
        let%lwt user_communities_res = match user_id_opt with
          | Some uid -> Membership_store.get_user_communities db (int_of_string uid)
          | None -> Lwt.return_ok []
        in
        let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
        let%lwt moderated_communities_res = match user_id_opt with
          | Some uid -> Moderator_store.get_moderated_communities db (int_of_string uid)
          | None -> Lwt.return_ok []
        in
        let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
        (* Fallback community: if the record is somehow missing, construct a minimal one
           from post fields so the page can still render without a 500. *)
        let community_for_page : Community_types.community = match community_res with
          | Ok (Some a) -> a
          | _ -> { id = post.community_id; slug = post.community_slug; name = post.community_slug;
                   description = None; rules = None; avatar_url = None; banner_url = None; allow_downvotes = true; sections_enabled = false; visibility = Community_types.Community_public; indexable = true;
                   is_network_community = false; onboarding_state = Community_types.Community_published; discoverable = true }
        in
        let%lwt noindex = Community_read_gate.thread_noindex db community_for_page post in
        (match comments_result, is_member_result with
        | Ok comments, Ok is_member ->
            let is_mod = match is_mod_res with Ok b -> b | _ -> false in
            let mod_usernames = match mods_res with Ok ms -> List.map (fun (u: User_store.user) -> u.username) ms | _ -> [] in
            Dream.html (Post_pages.post_page ?user:user_sess ~noindex ~is_member ~is_current_user_mod:is_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community:community_for_page ~user_communities ~moderated_communities user_post_votes user_comment_votes post comments request)
        | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load post data. Please try again later." ~alert_type:"error" ~return_url:"/" request))
        end

    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This post does not exist or has been deleted." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:user_sess ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* GET /c/:slug/t/:thread — canonical thread view inside the shell. ":thread" is "post_id-post_slug";
   post_id is the leading integer and is authoritative for lookup. A wrong community slug or a
   wrong/missing descriptive slug 301s to the canonical URL. Mirrors community_section_handler's
   sidebar/data load, then renders Thread_pages.thread_shell_page. *)
let view_thread_handler request =
  let user_sess = Dream.session_field request "username" in
  let user_id_opt = Dream.session_field request "user_id" in
  let community_slug = Dream.param request "slug" in
  let thread_param = Dream.param request "thread" in
  let post_id_opt =
    let s = match String.index_opt thread_param '-' with Some i -> String.sub thread_param 0 i | None -> thread_param in
    try Some (int_of_string s) with _ -> None
  in
  match post_id_opt with
  | None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"Invalid thread URL." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
  | Some post_id ->
  Dream.sql request (fun db ->
    match%lwt Post_store.get_post_by_id db post_id with
    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This thread does not exist or has been deleted." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:user_sess ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some post) ->
        let viewer_id = match user_id_opt with Some s -> (try int_of_string s with _ -> 0) | None -> 0 in
        (* CURRENT durable admin authority: every read gate on this page —
           destination context, origin thread, promotion provenance — takes
           this one value, and so does the Share affordance's own
           claim-plus-durable-SQL probe, which it can only make stricter. *)
        let%lwt is_admin = Admin_authority.current_admin_read_override db request in
        (* Shared Threads: a route slug that is NOT the post's own community
           may be an accepted destination context. One bounded read-model
           point query answers the placement facts (accepted, bound to this
           slug, origin currently public); the viewer's access to the
           destination is then the community's one EXISTING
           can_view_community rule over the loaded record — no third read
           rule. Every failure — no placement, an inactive placement, a
           private origin, an inaccessible private destination, a read
           error — takes the same fall-through into the pre-existing path
           below, whose observables (canonical 301 for viewers who may read
           the origin thread, one generic 404 otherwise) are identical for
           all of them, so no placement state can be inferred. *)
        let%lwt destination_context =
          if community_slug = post.community_slug then Lwt.return None
          else
            match%lwt
              Shared_thread_reading.resolve_destination_context db
                ~post_id:post.id ~destination_slug:community_slug
            with
            | Ok (Some ctx) -> (
                match%lwt
                  Community_store.get_community_by_id db
                    ctx.Shared_thread_reading.destination_community_id
                with
                | Ok (Some destination) ->
                    let%lwt viewable =
                      Community_read_gate.can_view_community db ~user_id:viewer_id ~admin_override:is_admin destination
                    in
                    Lwt.return (if viewable then Some (ctx, destination) else None)
                | _ -> Lwt.return None)
            | Ok None | Error _ -> Lwt.return None
        in
        (match destination_context with
        | Some (ctx, destination) ->
            (* One destination URL per thread: a wrong/missing descriptive
               slug 301s within the destination context, mirroring the
               canonical redirect. Server-built path — never a stored URL. *)
            let destination_path =
              Post_cards.canonical_thread_path destination.slug post.id post.title in
            let current_path = "/c/" ^ community_slug ^ "/t/" ^ thread_param in
            if current_path <> destination_path then
              Dream.redirect ~status:`Moved_Permanently request destination_path
            else begin
              let%lwt comments_result = Comment_store.get_comments db post.id in
              (* Membership here is the DESTINATION's (it feeds the join
                 CTA), but every canonical-content moderation input — the
                 mod flag, the moderator badges, the ban list — stays
                 ORIGIN-scoped: destination standing grants no canonical
                 controls, so a destination top mod reads as a plain
                 viewer. *)
              let%lwt is_member_result = match user_id_opt with
                | Some uid -> Membership_store.is_member db (int_of_string uid) destination.id
                | None -> Lwt.return (Ok false) in
              let%lwt user_post_votes = Handler_support.get_current_user_votes db request in
              let%lwt user_comment_votes = Handler_support.get_current_user_comment_votes db request in
              let%lwt is_mod_res = match user_id_opt with
                | Some uid -> Moderator_store.is_moderator db (int_of_string uid) post.community_id
                | None -> Lwt.return_ok false in
              let%lwt mods_res = Moderator_store.get_community_moderators db post.community_id in
              let%lwt admin_usernames_res = User_store.get_admin_usernames db in
              let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
              let%lwt banned_res = Community_ban_store.get_banned_users db post.community_id in
              let banned_usernames = match banned_res with Ok bs -> List.map (fun (u : User_store.user) -> u.username) bs | _ -> [] in
              let%lwt rail_communities = match user_id_opt with
                | Some uid -> (match%lwt Membership_store.get_user_communities db (int_of_string uid) with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
                | None -> Lwt.return [] in
              (* The destination shell's own navigation data. *)
              let%lwt channels = match%lwt Channel_store.get_channels_by_community db destination.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
              let%lwt sections = match%lwt Section_store.get_sections_with_stats db destination.id with
                | Ok stats -> Lwt.return (List.map (fun ((s : Section_store.community_section), _, _) -> s) stats)
                | Error _ -> Lwt.return [] in
              (* Promoted-conversation provenance, viewer-scoped exactly as
                 on the origin page. The source community IS the post's own
                 (promotion never crosses communities), which this context
                 guarantees is public — so the same rule admits it here. *)
              let%lwt thread_source =
                match%lwt Thread_source_store.get_thread_source db post.id with
                | Error _ | Ok (None, []) -> Lwt.return None
                | Ok (channel_opt, msgs) ->
                    (match channel_opt with
                     | None -> Lwt.return (Some (Thread_pages.Ts_visible (None, msgs)))
                     | Some (cslug, cname, src_community_id) ->
                         if src_community_id = post.community_id then
                           Lwt.return (Some (Thread_pages.Ts_visible (Some (cslug, cname), msgs)))
                         else
                           (match%lwt Community_store.get_community_by_id db src_community_id with
                            | Ok (Some src_community) ->
                                let%lwt src_ok = Community_read_gate.can_view_community db ~user_id:viewer_id ~admin_override:is_admin src_community in
                                Lwt.return (Some (if src_ok then Thread_pages.Ts_visible (Some (cslug, cname), msgs) else Thread_pages.Ts_private))
                            | _ -> Lwt.return (Some Thread_pages.Ts_private))) in
              (* The one comment-participation capability — the same SQL the
                 POST enforces. A failed probe hides the composer, never
                 errors the page. *)
              let%lwt can_comment =
                if viewer_id <= 0 then Lwt.return false
                else
                  match%lwt Shared_thread_reading.viewer_may_comment db ~user_id:viewer_id ~post_id:post.id with
                  | Ok can -> Lwt.return can
                  | Error _ -> Lwt.return false
              in
              let shared_context : Thread_pages.shared_thread_page_context =
                { stc_origin_name = ctx.Shared_thread_reading.origin_community_name;
                  stc_section = ctx.Shared_thread_reading.destination_section } in
              match comments_result, is_member_result with
              | Ok comments, Ok is_member ->
                  let is_mod = match is_mod_res with Ok b -> b | _ -> false in
                  let mod_usernames = match mods_res with Ok ms -> List.map (fun (u : User_store.user) -> u.username) ms | _ -> [] in
                  (* noindex always: the canonical <link> points at the
                     immutable origin URL and this page must never compete
                     with it in search engines (it stays followable — no
                     nofollow). No Share entry point here: the first MVP
                     permits requests only from the origin page. *)
                  Dream.html (Thread_pages.thread_shell_page ?user:user_sess ~noindex:true ~can_share:false ~can_comment
                    ~shared_context ~is_member ~is_current_user_mod:is_mod
                    ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities ~channels ~sections
                    ~community:destination ?thread_source ~user_post_votes ~user_comment_votes ~post ~comments request)
              | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load thread data. Please try again later." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
            end
        | None ->
        (* Slice C: gate BEFORE the canonical 301 below — redirecting leaks the private
           community's slug + thread title in the Location header. Resolve visibility from the
           post's community_id and deny with the SAME 404 as a missing thread. Fail closed. *)
        let%lwt gate_ok =
          match%lwt Community_store.get_community_by_id db post.community_id with
          | Ok (Some community) -> Community_read_gate.can_view_community db ~user_id:viewer_id ~admin_override:is_admin community
          | _ -> Lwt.return false
        in
        if not gate_ok then
          Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:user_sess ~title:"Not Found" ~message:"This thread does not exist or has been deleted." ~alert_type:"error" ~return_url:"/" request)
        else
        let canonical = Post_cards.canonical_thread_path post.community_slug post.id post.title in
        let current_path = "/c/" ^ community_slug ^ "/t/" ^ thread_param in
        if current_path <> canonical then
          (* Wrong community slug, or wrong/missing descriptive slug → 301 to canonical. *)
          Dream.redirect ~status:`Moved_Permanently request canonical
        else begin
          let%lwt comments_result = Comment_store.get_comments db post.id in
          let%lwt is_member_result = match user_id_opt with
            | Some uid -> Membership_store.is_member db (int_of_string uid) post.community_id
            | None -> Lwt.return (Ok false) in
          let%lwt user_post_votes = Handler_support.get_current_user_votes db request in
          let%lwt user_comment_votes = Handler_support.get_current_user_comment_votes db request in
          let%lwt is_mod_res = match user_id_opt with
            | Some uid -> Moderator_store.is_moderator db (int_of_string uid) post.community_id
            | None -> Lwt.return_ok false in
          let%lwt mods_res = Moderator_store.get_community_moderators db post.community_id in
          let%lwt admin_usernames_res = User_store.get_admin_usernames db in
          let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
          let%lwt banned_res = Community_ban_store.get_banned_users db post.community_id in
          let banned_usernames = match banned_res with Ok bs -> List.map (fun (u : User_store.user) -> u.username) bs | _ -> [] in
          let%lwt community_res = Community_store.get_community_by_slug db post.community_slug in
          let%lwt rail_communities = match user_id_opt with
            | Some uid -> (match%lwt Membership_store.get_user_communities db (int_of_string uid) with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
            | None -> Lwt.return [] in
          let%lwt channels = match%lwt Channel_store.get_channels_by_community db post.community_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
          let%lwt sections = match%lwt Section_store.get_sections_with_stats db post.community_id with
            | Ok stats -> Lwt.return (List.map (fun ((s : Section_store.community_section), _, _) -> s) stats)
            | Error _ -> Lwt.return [] in
          (* Promoted-conversation provenance, viewer-scoped. The DB returns raw rows; the
             VIEWER-visibility decision happens here: the source conversation is shown iff
             this viewer may read the source channel's community (can_view_community — the
             same predicate that gates the channel page itself), otherwise only a neutral
             Ts_private notice renders. Indexability plays no role: it is SEO-only and the
             thread's own noindex already follows the forum rules (thread_noindex above).
             In practice the source community IS the thread's community (promotion never
             crosses communities), and this viewer already passed that gate — the
             cross-community branch is defensive and fails closed to Ts_private. *)
          let%lwt thread_source =
            match%lwt Thread_source_store.get_thread_source db post.id with
            | Error _ | Ok (None, []) -> Lwt.return None
            | Ok (channel_opt, msgs) ->
                (match channel_opt with
                 | None -> Lwt.return (Some (Thread_pages.Ts_visible (None, msgs)))
                 | Some (cslug, cname, src_community_id) ->
                     if src_community_id = post.community_id then
                       Lwt.return (Some (Thread_pages.Ts_visible (Some (cslug, cname), msgs)))
                     else
                       (match%lwt Community_store.get_community_by_id db src_community_id with
                        | Ok (Some src_community) ->
                            let%lwt src_ok = Community_read_gate.can_view_community db ~user_id:viewer_id ~admin_override:is_admin src_community in
                            Lwt.return (Some (if src_ok then Thread_pages.Ts_visible (Some (cslug, cname), msgs) else Thread_pages.Ts_private))
                        | _ -> Lwt.return (Some Thread_pages.Ts_private))) in
          (* Fallback community kept for parity with view_post_handler; in practice the record exists. *)
          let community_for_page : Community_types.community = match community_res with
            | Ok (Some a) -> a
            | _ -> { id = post.community_id; slug = post.community_slug; name = post.community_slug;
                     description = None; rules = None; avatar_url = None; banner_url = None; allow_downvotes = true; sections_enabled = false; visibility = Community_types.Community_public; indexable = true;
                     is_network_community = false; onboarding_state = Community_types.Community_published; discoverable = true } in
          let%lwt noindex = Community_read_gate.thread_noindex db community_for_page post in
          (* Share entry point: decided in the shared-threads read model's SQL
             (author while member and unbanned, origin top_mod, or durable
             admin — false for a tombstoned post). Render gate only: the share
             route fully reauthorizes on GET, and a mere login never shows the
             action. Anonymous viewers skip the query; a failed probe hides
             the link rather than becoming an error path. *)
          let%lwt can_share =
            if viewer_id <= 0 then Lwt.return false
            else
              match%lwt
                Shared_thread_placement_read_model.viewer_may_share db
                  ~user_id:viewer_id ~session_global_admin:is_admin
                  ~post_id:post.id
              with
              | Ok can -> Lwt.return can
              | Error _ -> Lwt.return false
          in
          (* Composer gate: the one SQL participation capability POST
             /comments enforces (origin membership, or membership in a
             currently readable accepted destination — minus tombstone and
             every ban). A failed probe hides the composer, never errors. *)
          let%lwt can_comment =
            if viewer_id <= 0 then Lwt.return false
            else
              match%lwt Shared_thread_reading.viewer_may_comment db ~user_id:viewer_id ~post_id:post.id with
              | Ok can -> Lwt.return can
              | Error _ -> Lwt.return false
          in
          (* Closed creation-notice vocabulary (slice 4): only the two values
             the composer's own redirect writes render anything; every other
             ?shared= value is ignored. Resolved here, on the ORIGIN
             rendering only — the destination-context branch above never
             reads the parameter, so no destination page can display a
             creation outcome. *)
          let creation_notice = match Dream.query request "shared" with
            | Some "requested" -> Some Thread_pages.Creation_share_requested
            | Some "failed" -> Some Thread_pages.Creation_share_failed
            | _ -> None in
          (* Origin-side provenance for THIS canonical rendering only (the
             destination-context branch above never computes it): the post's
             currently publicly renderable accepted destinations, from the
             same batch read the global feed uses — one bounded query, and a
             failure degrades to no indicator, never an error page. *)
          let%lwt shared_with =
            match%lwt
              Shared_thread_reading.public_destinations_for_posts db
                ~post_ids:[ post.id ]
            with
            | Ok rows -> Lwt.return (List.map snd rows)
            | Error _ -> Lwt.return []
          in
          match comments_result, is_member_result with
          | Ok comments, Ok is_member ->
              let is_mod = match is_mod_res with Ok b -> b | _ -> false in
              let mod_usernames = match mods_res with Ok ms -> List.map (fun (u : User_store.user) -> u.username) ms | _ -> [] in
              Dream.html (Thread_pages.thread_shell_page ?user:user_sess ~noindex ~can_share ~can_comment ?creation_notice ~shared_with ~is_member ~is_current_user_mod:is_mod
                ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities ~channels ~sections
                ~community:community_for_page ?thread_source ~user_post_votes ~user_comment_votes ~post ~comments request)
          | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:user_sess ~title:"Error" ~message:"Failed to load thread data. Please try again later." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
        end)
  )

let delete_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in

      match%lwt Dream.form request with
      | `Ok form_data ->
          let post_id = try int_of_string (List.assoc_opt "post_id" form_data |> Option.value ~default:"") with _ -> 0 in
          if post_id = 0 then Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid post reference." ~alert_type:"error" ~return_url:"/" request)
          else

          Dream.sql request (fun db ->
            (* Only the AUTHORITY source changes here: a stale demoted-admin
               session is now evaluated exactly as a non-admin (author
               self-delete and the moderator path still decide on their own),
               and an unanswerable admin lookup deletes nothing at all. *)
            match%lwt Admin_authority.current_admin_bool db request with
            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok is_admin ->
            (* Fetch post upfront — needed for both mod-check and admin immunity. *)
            let%lwt post_opt =
              match%lwt Post_store.get_post_by_id db post_id with
              | Ok p -> Lwt.return p | _ -> Lwt.return None
            in
            let%lwt is_mod =
              if is_admin then Lwt.return false
              else match post_opt with
                | Some post ->
                    (match%lwt Moderator_store.is_moderator db user_id post.community_id with
                    | Ok b -> Lwt.return b
                    | _ -> Lwt.return false)
                | None -> Lwt.return false
            in
            (* Admin immunity: mods cannot delete content authored by global
               admins. [is_mod] is already false for a current global admin
               and for a plain author self-deleting, so only the moderator
               path pays this query — and only that path can be refused by it.
               The three outcomes stay distinct: immune, not immune, and
               unanswerable. Unanswerable is an internal failure rather than
               proof the author is an ordinary user, and it is settled HERE,
               above the image unlink and the delete below, so a failed lookup
               removes no file from disk and mutates no row. *)
            let%lwt target_immunity =
              if is_mod then match post_opt with
                | Some post -> User_store.is_user_admin db post.user_id
                | None -> Lwt.return (Ok false)
              else Lwt.return (Ok false)
            in
            match target_immunity with
            | Error e ->
                Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
            | Ok true ->
              Dream.respond ~status:`Forbidden "⛔ You cannot moderate an Admin."
            | Ok false ->
            (* Delete image from disk before nulling image_url in DB — prevents
               orphaned files that would still be served by the static file handler. *)
            let () =
              match post_opt with
              | Some post
                when is_admin || is_mod || post.user_id = user_id ->
                  (match post.image_url with
                  | Some image_url ->
                      (* Basename extraction avoids /static/uploads/... prefix mismatch
                         between the URL path stored in DB and the local filesystem. *)
                      let filename = Filename.basename image_url in
                      let physical_path = Filename.concat "static/uploads" filename in
                      Dream.log "Attempting to delete physical file: %s" physical_path;
                      (try Sys.remove physical_path
                       with Sys_error e -> Dream.log "Failed to delete file: %s" e)
                  | None -> ())
              | _ -> ()
            in
            let%lwt db_action =
              if is_admin || is_mod then Admin_store.admin_delete_post db ~label:"[removed by admin]" post_id
              else Post_store.soft_delete_post db post_id user_id
            in
            match db_action with
            | Ok () ->
                let target = Handler_support.safe_local_redirect request (match Dream.header request "Referer" with Some r -> r | None -> "/") in
                Dream.redirect request target
            | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
          )
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:"/" request)

(* Mod removal is a separate endpoint from /delete-post so that:
   (a) a reason is always required and stored, (b) the action is always
   attributed to a community moderator (not an admin shortcut), keeping
   mod_actions as a faithful community-level audit trail. *)
let mod_delete_post_handler request =
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      let slug = Dream.param request "slug" in
      let post_id = try int_of_string (Dream.param request "id") with _ -> 0 in
      if post_id = 0 then
        Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Bad Request" ~message:"Invalid post ID." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
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
                   to write the correct action_type in the audit log (admin_delete_post
                   vs delete_post), preventing admin spoofing via the community mod log.
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
                    (* Ownership gate: load the post and prove it belongs to the route
                       community BEFORE any side effect (disk, DB, modlog, notification).
                       A missing post and a post from another community get the same
                       neutral 404 — the response must not reveal that the numeric id
                       exists elsewhere, and admins on this community-scoped route obey
                       the same route-to-target relationship. *)
                    let not_found_here () =
                      Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Not Found" ~message:"This post does not exist in this community." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    in
                    (match%lwt Post_store.get_post_by_id db post_id with
                    | Error err ->
                        Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                    | Ok None -> not_found_here ()
                    | Ok (Some post) when post.community_id <> community.id -> not_found_here ()
                    | Ok (Some post) ->
                        (* The mutation re-proves the scope: id AND community_id, with
                           RETURNING as the match evidence. Ok false means the post
                           vanished or moved since the read above — still a neutral 404,
                           still zero side effects. *)
                        (match%lwt Admin_store.mod_delete_post db ~community_id:community.id post_id with
                        | Error err ->
                            Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
                        | Ok false -> not_found_here ()
                        | Ok true ->
                            (* Disk cleanup only after the scoped mutation confirmed the
                               target matched this community. *)
                            let () = match post.image_url with
                              | Some image_url ->
                                  let filename = Filename.basename image_url in
                                  let physical_path = Filename.concat "static/uploads" filename in
                                  Dream.log "Attempting to delete physical file: %s" physical_path;
                                  (try Sys.remove physical_path
                                   with Sys_error e -> Dream.log "Failed to delete file: %s" e)
                              | None -> ()
                            in
                            (* Admin acting without mod role: flag action_type and prefix reason
                               so the public mod_actions log explicitly shows "Admin Intervention". *)
                            let is_admin_override = is_admin && not is_community_mod in
                            let action_type = if is_admin_override then "admin_delete_post" else "delete_post" in
                            let logged_reason = if is_admin_override then "Admin Intervention: " ^ reason else reason in
                            let%lwt _ = Mod_log_store.log_action db community.id user_id action_type (Some post_id) logged_reason in
                            (* Notify the author from the row validated above — no re-query
                               of the tombstoned row. *)
                            let%lwt _ =
                              let msg = "Your post was removed by a moderator. Reason: " ^ reason in
                              Notification_store.create_notif db post.user_id (Some post_id) "mod_action" msg
                            in
                            Dream.redirect request ("/c/" ^ slug)))
          )
      | _ ->
          Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Invalid form submission." ~alert_type:"error" ~return_url:("/c/" ^ slug) request)
