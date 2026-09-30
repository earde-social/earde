(* GET /c/:slug/ch/:channel_slug — minimal SSR chat channel inside the shell. Mirrors
   community_section_handler: resolve community → channel, then load the sidebar data
   (channels + sections), the recent messages (author-resolved for render), membership,
   and the rail. No realtime here — see Chat_pages.community_channel_shell_page. *)
let community_channel_handler request =
  let community_slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> (try int_of_string id with _ -> 0) | None -> 0 in
  Dream.sql request (fun db ->
    (* CURRENT durable admin authority for the private-community read gate.
       The channel page mints a realtime token further down, so this gate is
       also what stands between a stale claim and a live socket. *)
    let%lwt is_admin = Admin_authority.current_admin_read_override db request in
    match%lwt Community_store.get_community_by_slug db community_slug with
    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some community) ->
        (* Read before the access check (see Realtime_generation): a
           revocation committing in between either fails the check or
           leaves the token on a topic nothing is published to anymore. *)
        let%lwt generation = Realtime_generation.current db ~community_id:community.id in
        let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
        if not authorized then Community_read_gate.community_not_found ?user request
        else
        match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
        | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
        | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
        | Ok (Some channel) ->
            let%lwt channels = match%lwt Channel_store.get_channels_by_community db community.id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return [] in
            let%lwt sections = match%lwt Section_store.get_sections_by_community db community.id with Ok ss -> Lwt.return ss | Error _ -> Lwt.return [] in
            (* Reverse navigation (?source_thread=<post_id>): instead of the last-50 tail,
               SSR a bounded window anchored on the promoted thread's earliest surviving
               source message. Promotion only ever selects from a ±10-message window
               around the seed, so 25-before + 60-after always covers the full span plus
               tail context — deliberately NOT general chat-history pagination. Any
               failure (bad id, unknown thread, thread from another channel, no surviving
               sources) falls back silently to the normal page. *)
            let%lwt source_focus, messages =
              let normal () =
                let%lwt ms = match%lwt Chat_store.get_recent_messages_with_authors db channel.id 50 with Ok ms -> Lwt.return ms | Error _ -> Lwt.return [] in
                Lwt.return (None, ms) in
              match Chat_pages.Start_thread.parse_source_thread (Dream.query request "source_thread") with
              | None -> normal ()
              | Some post_id ->
                  (match%lwt Thread_source_store.get_source_span_for_thread db post_id with
                   | Ok (Some (source_channel_id, post_title, (first :: _ as ids))) when source_channel_id = channel.id ->
                       let latest = List.fold_left (fun _ id -> id) first ids in
                       let%lwt before = match%lwt Chat_store.get_messages_before_id_with_authors db channel.id first 25 with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
                       let%lwt after = match%lwt Chat_store.get_messages_after_id_with_authors db channel.id (Int64.sub first 1L) 60 with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
                       (* Trim the tail to ~25 rows past the last source message so the
                          window stays tight even when the span sits deep in history. *)
                       let rec trim_after kept = function
                         | [] -> List.rev kept
                         | ((m : Chat_store.chat_message), _) as row :: rest ->
                             if m.id <= latest then trim_after (row :: kept) rest
                             else
                               let rec take n acc = function
                                 | [] -> List.rev acc
                                 | _ when n <= 0 -> List.rev acc
                                 | r :: rs -> take (n - 1) (r :: acc) rs in
                               List.rev_append kept (row :: take 24 [] rest) in
                       Lwt.return (Some (post_id, post_title, ids), before @ trim_after [] after)
                   | _ -> normal ()) in
            let%lwt is_member = if user_id > 0 then (match%lwt Membership_store.is_member db user_id community.id with Ok b -> Lwt.return b | Error _ -> Lwt.return false) else Lwt.return false in
            let%lwt rail_communities = if user_id > 0 then (match%lwt Membership_store.get_user_communities db user_id with Ok cs -> Lwt.return cs | Error _ -> Lwt.return []) else Lwt.return [] in
            (* All thread-source links for the channel -> the renderer marks seeds
               ("Thread ->") and context references ("Referenced in ->") per message in
               one query. can_start mirrors the composer's gate (a member); the
               start-thread form re-checks every permission server-side. *)
            let%lwt thread_links = match%lwt Thread_source_store.get_thread_links_for_channel db channel.id with Ok l -> Lwt.return l | Error _ -> Lwt.return [] in
            let can_start = is_member in
            let realtime_token =
              match user, user_id > 0, generation with
              | Some username, true, Ok generation ->
                  Realtime_token.create_for_topic
                    ~user_id
                    ~username
                    ~topic:(Realtime_generation.topic ~channel_id:channel.id ~generation)
                    ~shared_cursors:
                      (Features.shared_cursors_enabled
                         ~community_slug:community.slug)
              | Some _, true, Error e ->
                  (* The page stays useful without live updates. *)
                  Logs.err (fun m -> m "realtime generation lookup failed; no token: %s" e);
                  None
              | _ -> None
            in
            Dream.html (Chat_pages.community_channel_shell_page ?user ?realtime_token ~noindex:(Community_read_gate.child_noindex community ~child_indexable:channel.indexable) ~is_member ~can_start ~thread_links ?source_focus ~rail_communities ~channels ~sections ~channel ~messages ~community request)
  )

let int64_param_default name default request =
  match Dream.query request name with
  | None -> default
  | Some raw ->
      try Int64.of_string raw with _ -> default

let json_string s =
  `String s

let json_int i =
  `Int i

let json_int64 i =
  `Intlit (Int64.to_string i)

let chat_message_json
    ~(channel_id : int)
    ~(community_id : int)
    ?(thread_id : int option)
    ((message : Chat_store.chat_message), (author : string option)) =
  let username =
    match author with
    | Some username -> username
    | None -> "[deleted]"
  in
  (* thread_id/deleted mirror the SSR "Start thread" gate so a catch-up message renders the
     same affordance: the client hides Start thread when the message is deleted or already
     seeds a thread (thread_id present). Live new_msg events omit these — a brand-new message
     is never deleted or promoted — so their absence reads correctly as "still startable". *)
  `Assoc
    [ ("v", `Int 1)
    ; ("type", `String "chat_message_created")
    ; ("id", json_int64 message.id)
    ; ("channel_id", json_int channel_id)
    ; ("community_id", json_int community_id)
    ; ("user_id",
        (match message.user_id with
         | Some user_id -> json_int user_id
         | None -> `Null))
    ; ("username", json_string username)
    ; ("content",
        (match message.deleted_at with
         | Some _ -> `String "[message deleted]"
         | None -> json_string message.content))
      (* Minute precision everywhere a chat row is serialized, matching the SSR
         renderer, so live, catch-up and composer-response rows display alike. *)
    ; ("created_at", json_string (Chat_pages.Start_thread.minute_of_ts message.created_at))
    ; ("deleted", `Bool (message.deleted_at <> None))
    ; ("thread_id",
        (match thread_id with
         | Some pid -> json_int pid
         | None -> `Null))
    ]

(* ---- Chat composer JSON contract -----------------------------------------
   The chat composer submits over fetch with "Accept: application/json"; the
   no-JS fallback stays an ordinary form POST and keeps its redirect/HTML
   responses. These helpers are pure and exposed for tests so the negotiation
   rule, the validation boundary and the error shape cannot silently drift. *)
module Chat_api = struct
  (* A submission opts into JSON by sending an Accept value that mentions
     application/json; browser navigation Accept headers never do. *)
  let wants_json (accept : string option) =
    match accept with
    | None -> false
    | Some value ->
        let value = String.lowercase_ascii value in
        let needle = "application/json" in
        let nlen = String.length needle in
        let vlen = String.length value in
        let rec scan i =
          i + nlen <= vlen
          && (String.sub value i nlen = needle || scan (i + 1))
        in
        scan 0

  let max_content_length = 4000

  (* One validation for both response modes: trimmed content or a closed error
     variant, so the JSON and HTML paths always agree on what is sendable. *)
  let validate_content (raw : string) =
    let content = String.trim raw in
    if content = "" then Error `Empty
    else if String.length content > max_content_length then Error `Too_long
    else Ok content

  (* Client-safe error body: a stable code plus display copy only — never an
     exception string, SQL error or anything else internal. *)
  let error_json ~code ~message =
    Yojson.Safe.to_string
      (`Assoc [ ("error", `String code); ("message", `String message) ])

  let internal_error_json =
    error_json ~code:"internal" ~message:"Something went wrong. Please try again."
end

let channel_messages_json_handler request =
  let slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let after_id = int64_param_default "after_id" 0L request in
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0)
    | None -> 0
  in

  Dream.sql request (fun db ->
  (* CURRENT durable admin authority for the private-community read gate. *)
  let%lwt is_admin = Admin_authority.current_admin_read_override db request in
  match%lwt Community_store.get_community_by_slug db slug with
  | Error e ->
      Logs.err (fun m -> m "channel_messages_json: community lookup failed: %s" e);
      Dream.respond ~status:`Internal_Server_Error "Internal server error"

  (* Privacy: authorize BEFORE resolving the channel, and use the same not-found
     response as community_channel_handler for both a missing community AND a denied
     private read. A distinguishable 403 (or a channel 404 reached pre-auth) would let
     an unauthorized user enumerate private communities and their channel slugs. *)
  | Ok None -> Community_read_gate.community_not_found ?user request

  | Ok (Some community) ->
      let%lwt can_view = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
      if not can_view then Community_read_gate.community_not_found ?user request
      else (
        match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
        | Error e ->
            Logs.err (fun m -> m "channel_messages_json: channel lookup failed: %s" e);
            Dream.respond ~status:`Internal_Server_Error "Internal server error"

        | Ok None ->
            Dream.respond ~status:`Not_Found "Channel not found"

        | Ok (Some channel) ->
            match%lwt Chat_store.get_messages_after_id_with_authors db channel.id after_id 100 with
            | Error e ->
                Logs.err (fun m -> m "channel_messages_json: messages lookup failed: %s" e);
                Dream.respond ~status:`Internal_Server_Error "Internal server error"

            | Ok messages ->
                (* Seed-thread lookup so catch-up marks already-promoted messages, matching SSR.
                   Best-effort: on error we fall back to [] (no thread_id), i.e. Start thread may
                   briefly reappear on a promoted message until reload — never a wrong link. *)
                let%lwt thread_links =
                  match%lwt Thread_source_store.get_thread_links_for_channel db channel.id with
                  | Ok l -> Lwt.return l
                  | Error _ -> Lwt.return []
                in
                let seed_thread_id (mid : int64) : int option =
                  List.find_map
                    (fun (m_id, pid, _title, is_seed) ->
                      if is_seed && m_id = mid then Some pid else None)
                    thread_links
                in
                let json =
                  `Assoc
                    [ ("messages",
                        `List
                          (List.map
                             (fun ((m : Chat_store.chat_message), _ as row) ->
                                chat_message_json
                                  ~channel_id:channel.id
                                  ~community_id:community.id
                                  ?thread_id:(seed_thread_id m.id)
                                  row)
                             messages))
                    ]
                in
                Dream.json (Yojson.Safe.to_string json)))

(* GET /c/:slug/ch/:channel_slug/realtime-token — fresh websocket token for the chat page's
   JS, so a socket can reconnect after the initial token's expiry without a page reload.
   Authentication is required BEFORE any lookup: every slug gets the same 401 for an anonymous
   caller, so nothing is enumerable from this endpoint. Authenticated callers then follow
   channel_messages_json_handler's exact privacy order (community → can_view_community →
   channel), reusing community_not_found for denied private reads. The token itself is minted
   by the same Realtime_token.create_for_topic used at page render — no second signing path —
   and only ever travels in this response body, never in a URL or a log line. *)
let realtime_token_handler request =
  let slug = Dream.param request "slug" in
  let channel_slug = Dream.param request "channel_slug" in
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0)
    | None -> 0
  in
  match user, user_id > 0 with
  | None, _ | _, false ->
      Dream.json ~status:`Unauthorized {|{"error":"unauthorized"}|}
  | Some username, true ->
      Dream.sql request (fun db ->
        (* CURRENT durable admin authority: a stale demoted-admin claim must
           not be able to mint a token for a private community's channel. *)
        let%lwt is_admin = Admin_authority.current_admin_read_override db request in
        match%lwt Community_store.get_community_by_slug db slug with
        | Error e ->
            Logs.err (fun m -> m "realtime_token: community lookup failed: %s" e);
            Dream.respond ~status:`Internal_Server_Error "Internal server error"
        | Ok None -> Community_read_gate.community_not_found ?user request
        | Ok (Some community) ->
            (* Before the access check, as at page render. *)
            match%lwt Realtime_generation.current db ~community_id:community.id with
            | Error e ->
                Logs.err (fun m -> m "realtime_token: generation lookup failed: %s" e);
                Dream.respond ~status:`Internal_Server_Error "Internal server error"
            | Ok generation ->
            let%lwt can_view = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
            if not can_view then Community_read_gate.community_not_found ?user request
            else (
              match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
              | Error e ->
                  Logs.err (fun m -> m "realtime_token: channel lookup failed: %s" e);
                  Dream.respond ~status:`Internal_Server_Error "Internal server error"
              | Ok None -> Dream.respond ~status:`Not_Found "Channel not found"
              | Ok (Some channel) ->
                  let topic = Realtime_generation.topic ~channel_id:channel.id ~generation in
                  (* Capability recomputed from the community on every refresh —
                     never copied from the old token or any client input. *)
                  let shared_cursors =
                    Features.shared_cursors_enabled ~community_slug:community.slug
                  in
                  match Realtime_token.create_for_topic ~user_id ~username ~topic ~shared_cursors with
                  | None ->
                      (* Signing secret not configured: realtime is off for this
                         deployment; the client stops proactive refresh cleanly. *)
                      Dream.json ~status:`Service_Unavailable
                        {|{"error":"realtime unavailable"}|}
                  | Some token ->
                      Dream.json
                        (Yojson.Safe.to_string
                           (`Assoc
                             [ ("token", `String token)
                             ; ("expires_in", `Int Realtime_token.default_ttl_seconds)
                             ]))))

(* POST /messages — send a chat message. Two response modes over one endpoint:
   an ordinary form POST (no-JS fallback) keeps the historical redirect/HTML
   contract, while the chat page's fetch submission (Accept: application/json)
   receives JSON — a canonical new_msg-shaped row on success, a safe
   code+message body on failure — so the page never navigates. Validation,
   authorization and persistence are identical for both modes: CSRF
   auto-validated by Dream.form, hidden community_slug + channel_slug (not a
   raw id) re-resolved and re-validated server-side, safety gates mirroring
   create_post_handler (global ban → membership → local ban), Postgres write
   first, gateway publish best-effort after. *)
let send_message_handler request =
  let respond_json = Chat_api.wants_json (Dream.header request "Accept") in
  let json_error status ~code ~message =
    Dream.json ~status (Chat_api.error_json ~code ~message)
  in
  match Dream.session_field request "user_id" with
  | None ->
      if respond_json then
        json_error `Unauthorized ~code:"unauthorized"
          ~message:"Your session has ended. Reload the page and log in."
      else Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = try int_of_string uid_str with _ -> 0 in
      let uname = Dream.session_field request "username" in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let community_slug = List.assoc_opt "community_slug" form_data |> Option.value ~default:"" in
          let channel_slug = List.assoc_opt "channel_slug" form_data |> Option.value ~default:"" in
          let raw_content = List.assoc_opt "content" form_data |> Option.value ~default:"" in
          let back_url = Printf.sprintf "/c/%s/ch/%s" community_slug channel_slug in
          let internal_error e =
            Logs.err (fun m -> m "send_message: %s" e);
            if respond_json then
              Dream.json ~status:`Internal_Server_Error Chat_api.internal_error_json
            else
              Dream.html (Site_pages.msg_page ?user:uname ~title:"Error" ~message:Handler_support.generic_db_error ~alert_type:"error" ~return_url:"/" request)
          in
          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Community_store.get_community_by_slug db community_slug with
            | Ok (Some community) ->
                (match%lwt Channel_store.get_channel_by_slug db channel_slug community.id with
                 | Ok (Some channel) ->
                     (* Fail closed: an unreadable global-ban state is a storage
                        failure, never "not banned" — no message is persisted
                        and nothing is published to the gateway. *)
                     (match%lwt Admin_store.is_globally_banned db user_id with
                     | Error e -> internal_error e
                     | Ok true ->
                       (if respond_json then
                          json_error `Forbidden ~code:"forbidden"
                            ~message:"Your account has been permanently banned from Earde."
                        else
                          Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:uname ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/" request))
                     | Ok false -> begin
                       match%lwt Membership_store.is_member db user_id community.id with
                       | Ok true ->
                           (match%lwt Community_ban_store.is_banned db user_id community.id with
                            | Ok true ->
                                if respond_json then
                                  json_error `Forbidden ~code:"forbidden"
                                    ~message:"You are banned from this community."
                                else
                                  Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:uname ~title:"Banned from Community" ~message:"You are banned from this community." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                            (* Same fail-closed rule as the global gate above:
                               the local-ban read failing is not permission. *)
                            | Error e -> internal_error e
                            | Ok false ->
                                (match Chat_api.validate_content raw_content with
                                 | Error `Empty ->
                                     if respond_json then
                                       json_error `Bad_Request ~code:"empty" ~message:"Message is empty."
                                     else Dream.redirect request (Handler_support.safe_local_redirect request back_url)
                                 | Error `Too_long ->
                                     if respond_json then
                                       json_error `Bad_Request ~code:"too_long"
                                         ~message:"Messages cannot exceed 4000 characters."
                                     else
                                       Dream.html (Site_pages.msg_page ?user:uname ~title:"Message too long" ~message:"Messages cannot exceed 4000 characters." ~alert_type:"error" ~return_url:back_url request)
                                 | Ok content ->
                                  (match%lwt Chat_store.send_message db channel.id user_id content with
                                   | Ok message ->
                                       (* INSERT ... RETURNING hands back the canonical
                                          persisted row (Postgres id and created_at) in
                                          the insert round-trip, so both the publish and
                                          the JSON success body come straight from what
                                          was stored — no read-back, no synthesized
                                          fields. Username joins from the already
                                          authenticated session. *)
                                       let username = Option.value uname ~default:"[unknown]" in
                                       (* Read after the insert committed: a message created
                                          after someone lost access must only reach the topic
                                          their old token cannot join. If the read fails the
                                          live publish is skipped; the message is durable and
                                          clients catch up over HTTP. *)
                                       let%lwt generation =
                                         Realtime_generation.current db ~community_id:community.id
                                       in
                                       (match generation with
                                        | Ok generation ->
                                       Lwt.async (fun () ->
                                           Realtime.publish_chat_message
                                             ~topic:(Realtime_generation.topic ~channel_id:channel.id ~generation)
                                             ~channel_id:channel.id
                                             ~community_id:community.id
                                             ~message_id:message.id
                                             ~user_id
                                             ~username
                                             ~content
                                             ~created_at:(Chat_pages.Start_thread.minute_of_ts message.created_at))
                                        | Error e ->
                                            Logs.err (fun m -> m "realtime generation lookup failed; live publish skipped: %s" e));
                                       (* One capture point ahead of the
                                          respond_json split, so JSON and
                                          redirect modes each emit exactly
                                          once, never twice. *)
                                       record (fun () ->
                                           Analytics.capture_if_consented request
                                             ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                             (Analytics.Chat_message_sent
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
                                                  message_id = message.id;
                                                  content_length = String.length content;
                                                  response_mode =
                                                    (if respond_json then Analytics.Response_json
                                                     else Analytics.Response_redirect);
                                                }));
                                       if respond_json then
                                         Dream.json
                                           (Yojson.Safe.to_string
                                              (chat_message_json
                                                 ~channel_id:channel.id
                                                 ~community_id:community.id
                                                 (message, Some username)))
                                       else Dream.redirect request (Handler_support.safe_local_redirect request back_url)
                                   | Error e ->
                                       if respond_json then internal_error e
                                       else Dream.html (Site_pages.msg_page ?user:uname ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:back_url request))))
                       | Ok false ->
                           if respond_json then
                             json_error `Forbidden ~code:"not_member" ~message:"Join this community to chat."
                           else
                             Dream.respond ~status:`Forbidden (Site_pages.msg_page ?user:uname ~title:"Not a Member" ~message:"You must join this community to chat." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                       | Error e -> internal_error e
                     end)
                 | Ok None ->
                     if respond_json then
                       json_error `Not_Found ~code:"not_found" ~message:"This channel does not exist."
                     else Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:uname ~title:"Not Found" ~message:"This channel does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
                 | Error e -> internal_error e)
            | Ok None ->
                if respond_json then
                  json_error `Not_Found ~code:"not_found" ~message:"This community does not exist."
                else Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user:uname ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
            | Error e -> internal_error e))
      | _ ->
          (* Dream.form failure: missing/stale CSRF or a non-form body. The fetch
             path surfaces it as retry-after-reload guidance — a chat tab older
             than the CSRF token lifetime lands here. *)
          if respond_json then
            json_error `Bad_Request ~code:"stale_form"
              ~message:"This page is out of date. Reload and try again."
          else
            Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:uname ~title:"Form Error" ~message:"There was a problem with your submission. Please try again." ~alert_type:"error" ~return_url:"/" request)
