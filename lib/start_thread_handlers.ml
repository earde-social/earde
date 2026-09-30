(* ---- Start thread from chat -------------------------------------------------
   Crystallize a chat conversation into a durable forum thread. A seed message plus
   nearby context messages become a normal post; real provenance lives in
   thread_source_messages. UI says "Start thread", never "Promote". *)

(* Nearby-message window (10 before + 10 after the seed) and the cap on how many
   source messages a thread may carry (seed included), per the approved product rules. *)
let start_thread_window = 10

let start_thread_max_total = 10

(* Default section: the community's 'general' section (guaranteed present by the
   default-structure invariant) else the first in query order; 0 when sectionless. *)
let default_thread_section_id sections =
  match List.find_opt (fun (s : Section_store.community_section) -> s.slug = "general") sections with
  | Some s -> s.section_id
  | None -> (match sections with (s : Section_store.community_section) :: _ -> s.section_id | [] -> 0)

(* Permission gate (decision: any logged-in, non-banned community member; mods/admins
   too). Mirrors send_message's order: global ban -> local ban -> membership -> mod/admin. *)
type start_perm = Start_allowed | Start_not_member | Start_banned | Start_error of string

(* [admin_override] must be CURRENT durable admin authority, never the raw
   session claim — same contract, and same deliberately different spelling, as
   can_view_community. *)
let check_start_permission db ~user_id ~admin_override ~community_id =
  let is_admin = admin_override in
  (* Every storage failure in this gate — global ban included — is Start_error,
     never a silent "not banned": callers turn Start_error into a generic 500
     before any thread is created. *)
  match%lwt Admin_store.is_globally_banned db user_id with
  | Error e -> Lwt.return (Start_error e)
  | Ok true -> Lwt.return Start_banned
  | Ok false -> begin
    match%lwt Community_ban_store.is_banned db user_id community_id with
    | Ok true -> Lwt.return Start_banned
    | Error e -> Lwt.return (Start_error e)
    | Ok false ->
        (match%lwt Membership_store.is_member db user_id community_id with
         | Ok true -> Lwt.return Start_allowed
         | Error e -> Lwt.return (Start_error e)
         | Ok false ->
             if is_admin then Lwt.return Start_allowed
             else
               (match%lwt Moderator_store.is_moderator db user_id community_id with
                | Ok true -> Lwt.return Start_allowed
                | Ok false -> Lwt.return Start_not_member
                | Error e -> Lwt.return (Start_error e)))
  end

(* GET /c/:slug/ch/:channel_slug/messages/:message_id/start-thread — render the form.
   The seed is fetched with its author by reading the channel window inclusively
   (after id-1 returns the seed first), so we never need a separate user lookup. *)
let start_thread_form_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let message_id_str = Dream.param request "message_id" in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let user = Dream.session_field request "username" in
      (match Int64.of_string_opt message_id_str with
       | None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Invalid message reference." ~alert_type:"error" ~return_url:channel_url request)
       | Some message_id ->
           Dream.sql request (fun db ->
             (* CURRENT durable admin authority: feeds the private read gate,
                the start-permission override and the sidebar affordance. *)
             let%lwt is_admin = Admin_authority.current_admin_read_override db request in
             match%lwt Community_store.get_community_by_slug db community_slug with
             | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
             | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
             | Ok (Some community) ->
                 (* A private community is hidden — a non-authorized viewer gets the
                    same 404 as a missing community, BEFORE any membership-specific 403 below.
                    This does not broaden who may create a thread (check_start_permission still
                    runs for authorized viewers). *)
                 let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
                 if not authorized then Community_read_gate.community_not_found ?user request
                 else
                 (match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
                  | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                  | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some channel) ->
                      (match%lwt Chat_store.get_message_by_id db message_id with
                       | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                       | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"That message does not exist." ~alert_type:"error" ~return_url:channel_url request)
                       | Ok (Some (seed : Chat_store.chat_message)) ->
                           if seed.channel_id <> channel.id || seed.deleted_at <> None then
                             Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"That message is not available to start a thread from." ~alert_type:"error" ~return_url:channel_url request)
                           else
                             (match%lwt check_start_permission db ~user_id ~admin_override:is_admin ~community_id:community.id with
                              | Start_banned -> Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot start threads in this community." ~alert_type:"error" ~return_url:channel_url request)
                              | Start_not_member -> Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Join to start a thread" ~message:"You must be a member of this community to start a thread." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                              | Start_error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                              | Start_allowed ->
                                  (match%lwt Thread_source_store.get_seed_thread_for_message db message_id with
                                   | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                   | Ok (Some (post_id, title, cslug)) ->
                                       Dream.html (Site_pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Post_cards.canonical_thread_path cslug post_id title) request)
                                   | Ok None ->
                                       let%lwt before = (match%lwt Chat_store.get_messages_before_id_with_authors db channel.id message_id start_thread_window with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                       let%lwt seed_and_after = (match%lwt Chat_store.get_messages_after_id_with_authors db channel.id (Int64.sub message_id 1L) (start_thread_window + 1) with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                       let%lwt sections = (match%lwt Section_store.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return []) in
                                       (* Exclude soft-deleted from context; the seed is guaranteed non-deleted above. *)
                                       let candidates = List.filter (fun ((m : Chat_store.chat_message), _) -> m.deleted_at = None) (before @ seed_and_after) in
                                       let default_title = Chat_pages.Start_thread.derive_title seed.content in
                                       (* The introduction starts EMPTY — no generated transcript. The selected
                                          messages render on the thread from their persisted relations; the
                                          textarea carries only text the curator deliberately writes. *)
                                       let def_section_id = default_thread_section_id sections in
                                       (* Launch-chrome data, loaded only after every gate
                                          above passed — a hidden private community, a missing/foreign
                                          seed, a banned viewer and a non-member never touch the
                                          viewer's memberships or the channel list. Each degrades to an
                                          empty list on error rather than blocking the form. can_manage
                                          mirrors the report form's gate (admin || moderator) and only
                                          picks the sidebar Settings visibility; the settings handler
                                          re-checks. *)
                                       let%lwt channels = (match%lwt Channel_store.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                       let%lwt rail_communities = (match%lwt Membership_store.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                       let%lwt can_manage =
                                         if is_admin then Lwt.return true
                                         else (match%lwt Moderator_store.is_moderator db user_id community.id with
                                           | Ok b -> Lwt.return b
                                           | _ -> Lwt.return false)
                                       in
                                       Dream.html (Post_pages.start_thread_form ?user ~rail_communities ~channels ~can_manage ~community ~channel ~seed_id:message_id ~candidates ~sections ~default_section_id:def_section_id ~default_title ~default_body:"" request)))))))

(* POST same path — validate everything server-side (never trust the client), force the
   seed into the source set, then create the thread + provenance atomically. *)
let start_thread_create_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let message_id_str = Dream.param request "message_id" in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
  match Dream.session_field request "user_id" with
  | None -> Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let user = Dream.session_field request "username" in
      (match%lwt Dream.form request with
       | `Ok form_data ->
           (match Int64.of_string_opt message_id_str with
            | None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"Invalid message reference." ~alert_type:"error" ~return_url:channel_url request)
            | Some message_id ->
                Analytics_handlers.with_analytics_after_sql (fun record ->
                Dream.sql request (fun db ->
                  (* CURRENT durable admin authority: feeds the private read
                     gate and the start-permission override. *)
                  let%lwt is_admin = Admin_authority.current_admin_read_override db request in
                  match%lwt Community_store.get_community_by_slug db community_slug with
                  | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
                  | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                  | Ok (Some community) ->
                      (* Private community hidden — non-authorized viewer gets 404, not
                         the membership 403 below. check_start_permission still gates creation. *)
                      let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
                      if not authorized then Community_read_gate.community_not_found ?user request
                      else
                      (match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
                       | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                       | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
                       | Ok (Some channel) ->
                           (match%lwt Chat_store.get_message_by_id db message_id with
                            | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                            | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"That message does not exist." ~alert_type:"error" ~return_url:channel_url request)
                            | Ok (Some (seed : Chat_store.chat_message)) ->
                                if seed.channel_id <> channel.id || seed.deleted_at <> None then
                                  Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"That message is not available to start a thread from." ~alert_type:"error" ~return_url:channel_url request)
                                else
                                  (match%lwt check_start_permission db ~user_id ~admin_override:is_admin ~community_id:community.id with
                                   | Start_banned -> Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Not Allowed" ~message:"You cannot start threads in this community." ~alert_type:"error" ~return_url:channel_url request)
                                   | Start_not_member -> Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user ~title:"Join to start a thread" ~message:"You must be a member of this community to start a thread." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                                   | Start_error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                   | Start_allowed ->
                                       (match%lwt Thread_source_store.get_seed_thread_for_message db message_id with
                                        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:channel_url request)
                                        | Ok (Some (pid, ttl, cslug)) ->
                                            Dream.html (Site_pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Post_cards.canonical_thread_path cslug pid ttl) request)
                                        | Ok None ->
                                            let%lwt before = (match%lwt Chat_store.get_messages_before_id_with_authors db channel.id message_id start_thread_window with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                            let%lwt seed_and_after = (match%lwt Chat_store.get_messages_after_id_with_authors db channel.id (Int64.sub message_id 1L) (start_thread_window + 1) with Ok l -> Lwt.return l | Error _ -> Lwt.return []) in
                                            let%lwt sections = (match%lwt Section_store.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return []) in
                                            let candidates = List.filter (fun ((m : Chat_store.chat_message), _) -> m.deleted_at = None) (before @ seed_and_after) in
                                            let valid = List.map (fun ((m : Chat_store.chat_message), _) -> m.id) candidates in
                                            let def_section_id = default_thread_section_id sections in
                                            let title = String.trim (Option.value (List.assoc_opt "title" form_data) ~default:"") in
                                            let content_raw = String.trim (Option.value (List.assoc_opt "content" form_data) ~default:"") in
                                            let content = if content_raw = "" then None else Some content_raw in
                                            let section_id_str = Option.value (List.assoc_opt "section_id" form_data) ~default:"" in
                                            let selected = Chat_pages.Start_thread.parse_selected_ids form_data in
                                            let context = Chat_pages.Start_thread.normalize_selection ~seed:message_id ~max_total:start_thread_max_total ~valid selected in
                                            let rerender ?(error="") () =
                                              (* Launch-chrome data, loaded only when a validation
                                                 state actually re-renders the form — the success path keeps
                                                 its exact query set. Same post-gate position and degradation
                                                 as the GET handler. *)
                                              let%lwt channels = (match%lwt Channel_store.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                              let%lwt rail_communities = (match%lwt Membership_store.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) in
                                              let%lwt can_manage =
                                                if is_admin then Lwt.return true
                                                else (match%lwt Moderator_store.is_moderator db user_id community.id with
                                                  | Ok b -> Lwt.return b
                                                  | _ -> Lwt.return false)
                                              in
                                              Dream.html (Post_pages.start_thread_form ?user ~error ~rail_communities ~channels ~can_manage ~community ~channel ~seed_id:message_id ~candidates ~sections ~default_section_id:def_section_id ~default_title:title ~default_body:content_raw request)
                                            in
                                            if title = "" then rerender ~error:"Please enter a title for the thread." ()
                                            else if String.length title > 300 then rerender ~error:"Title cannot exceed 300 characters." ()
                                            else begin
                                              let%lwt section_result =
                                                if not community.sections_enabled then Lwt.return (Ok None)
                                                else begin
                                                  let sid = try int_of_string section_id_str with _ -> 0 in
                                                  if sid = 0 then Lwt.return (Error "section_required")
                                                  else match%lwt Section_store.get_section_by_id db sid community.id with
                                                    | Ok (Some _) -> Lwt.return (Ok (Some sid))
                                                    | Ok None -> Lwt.return (Error "section_invalid")
                                                    | Error e -> Lwt.return (Error e)
                                                end
                                              in
                                              match section_result with
                                              | Error "section_required" -> rerender ~error:"Please select a section." ()
                                              | Error "section_invalid" -> rerender ~error:"The selected section is not valid for this community." ()
                                              | Error e -> rerender ~error:(Handler_support.db_error_message e) ()
                                              | Ok section_id ->
                                                  (match%lwt Thread_source_store.start_thread_from_chat db ~title ~content ~section_id ~community_id:community.id ~user_id ~channel_id:channel.id ~seed_message_id:message_id ~context_message_ids:context with
                                                   | Ok post_id ->
                                                       let%lwt _ = Community_user_stats_store.increment_local_post_count db user_id community.id in
                                                       (* Participant count from the candidate rows the selection was
                                                          already validated against — distinct non-tombstoned author ids
                                                          across the promoted messages; no extra query, no guessing. *)
                                                       let promoted_ids = message_id :: context in
                                                       let participant_count =
                                                         candidates
                                                         |> List.filter (fun ((m : Chat_store.chat_message), _) -> List.mem m.id promoted_ids)
                                                         |> List.filter_map (fun ((m : Chat_store.chat_message), _) -> m.user_id)
                                                         |> List.sort_uniq compare
                                                         |> List.length
                                                       in
                                                       record (fun () ->
                                                           Analytics.capture_if_consented request
                                                             ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                                             (Analytics.Conversation_promoted
                                                                {
                                                                  user_id;
                                                                  community_id = community.id;
                                                                  community_slug =
                                                                    Analytics_handlers.analytics_public_string
                                                                      community community.slug;
                                                                  channel_id = channel.id;
                                                                  channel_slug =
                                                                    Analytics_handlers.analytics_public_string
                                                                      community channel.slug;
                                                                  section_id;
                                                                  post_id;
                                                                  message_id;
                                                                  promoted_message_count = 1 + List.length context;
                                                                  promoted_participant_count = Some participant_count;
                                                                }));
                                                       Dream.redirect request ("/p/" ^ string_of_int post_id)
                                                   | Error _ ->
                                                       (* A racing double-submit trips the seed unique index. Re-check and
                                                          link the existing thread; otherwise a generic error (no raw DB text). *)
                                                       (match%lwt Thread_source_store.get_seed_thread_for_message db message_id with
                                                        | Ok (Some (pid, ttl, cslug)) ->
                                                            Dream.html (Site_pages.msg_page ?user ~title:"Thread already started" ~message:"This message has already been made into a thread." ~alert_type:"info" ~return_url:(Post_cards.canonical_thread_path cslug pid ttl) request)
                                                        | _ -> rerender ~error:"Could not start the thread. Please try again." ()))
                                            end)))))))
       | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:channel_url request))
