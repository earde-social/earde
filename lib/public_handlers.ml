(* === CORE FEED ===

   /feed is the only global feed surface; / and /all are redirects to it (see
   lib/app_routes.ml). The pre-/feed home handler and its warm-chrome renderer were
   removed with the rest of the legacy chrome. *)

(* /feed — the global Feed surface (shell-language, outside any one community). Reuses the same
   feed queries as home/all: "following" → personalized feed from joined communities, "all" →
   every public persistent post. No new query, no migration, no realtime. Default scope is
   following for logged-in users; guests are forced to "all" (no personalized feed without a
   session) and never see the toggle. *)
let feed ~page request =
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> int_of_string id
    | None -> 0
  in
  let is_logged_in = user_id > 0 in
  let sort_mode =
    match Dream.query request "sort" with
    | Some "new" -> Post_types.Newest
    | Some "top" -> Post_types.Top
    | Some "active" -> Post_types.Active
    | _ -> Post_types.Hot
  in
  let sort_str =
    match sort_mode with
    | Post_types.Newest -> "new"
    | Post_types.Top -> "top"
    | Post_types.Hot -> "hot"
    | Post_types.Active -> "active"
  in
  (* Guests can't have a personalized feed, so any scope coerces to "all" for them. *)
  let scope =
    match (Dream.query request "scope", is_logged_in) with
    | Some "all", _ -> "all"
    | _, false -> "all"
    | _ -> "following"
  in
  let limit = Public_pagination.page_size in
  let offset = Public_pagination.offset page in

  Dream.sql request (fun db ->
      let%lwt posts =
        if scope = "following" then
          Post_store.get_personalized_feed db user_id sort_mode limit offset
        else Post_store.get_all_posts db sort_mode limit offset
      in
      let%lwt user_votes =
        if user_id > 0 then User_store.get_user_post_votes db user_id
        else Lwt.return_ok []
      in
      let%lwt user_communities =
        if user_id > 0 then Membership_store.get_user_communities db user_id
        else Lwt.return_ok []
      in
      let%lwt admin_usernames_res = User_store.get_admin_usernames db in
      let admin_usernames =
        match admin_usernames_res with Ok l -> l | Error _ -> []
      in

      match (posts, user_votes, user_communities) with
      | Ok p, Ok v, Ok rail ->
          (* Origin-side Shared Threads provenance for exactly the page of
           canonical posts the feed query already selected: ONE bounded
           batch read (never per-post), grouped here into a
           post_id -> destinations map. Purely additive enrichment — the
           feed's selection, ordering, and pagination above are untouched —
           and a failure degrades to no indicators, never a failed page
           (the chat-provenance pattern). *)
          let%lwt shared_destinations =
            match%lwt
              Shared_thread_reading.public_destinations_for_posts db
                ~post_ids:(List.map (fun (post : Post_types.post) -> post.id) p)
            with
            | Ok rows ->
                let grouped =
                  List.fold_left
                    (fun acc (post_id, dest) ->
                      let existing =
                        Option.value ~default:[] (List.assoc_opt post_id acc)
                      in
                      (post_id, existing @ [ dest ])
                      :: List.remove_assoc post_id acc)
                    [] rows
                in
                Lwt.return grouped
            | Error _ -> Lwt.return []
          in
          Dream.html
            (Public_pages.feed_page ?user ~scope ~sort_mode:sort_str
               ~is_logged_in ~admin_usernames ~rail_communities:rail
               ~user_votes:v ~current_page:page ~shared_destinations p request)
      | Error e, _, _ | _, Error e, _ | _, _, Error e ->
          Dream.respond ~status:`Internal_Server_Error
            (Site_pages.msg_page ?user ~title:"Error"
               ~message:(Handler_support.db_error_message e)
               ~alert_type:"error" ~return_url:"/" request))

(* The page number is checked before any database work: past
   Public_pagination.max_page the request ends here with a 400. *)
let feed_handler request =
  match Public_pagination.parse (Dream.query request "page") with
  | Ok page -> feed ~page request
  | Error `Out_of_range ->
      Public_pagination.out_of_range
        ?user:(Dream.session_field request "username")
        ~return_url:"/feed" request

let search ~page request =
  let user = Dream.session_field request "username" in
  let user_id =
    match Dream.session_field request "user_id" with
    | Some id -> int_of_string id
    | None -> 0
  in

  let active_tab =
    match Dream.query request "t" with Some t -> t | None -> "posts"
  in
  (* Trim the query; whitespace-only is treated as empty. An empty query renders the search
     page's local prompt state (no redirect) so /search is a real, bookmarkable surface. No
     minimum length is imposed — a one-character query like /search?q=a still runs. *)
  let search_term =
    match Dream.query request "q" with Some q -> String.trim q | None -> ""
  in

  if search_term = "" then
    (* The empty-query prompt state needs no search data, but members still get
       their joined communities for the launch rail (same degrade-to-empty rule
       as notifications). Anonymous prompts stay DB-free. *)
    if user_id > 0 then
      Dream.sql request (fun db ->
          let%lwt rail_communities =
            match%lwt Membership_store.get_user_communities db user_id with
            | Ok cs -> Lwt.return cs
            | Error _ -> Lwt.return []
          in
          Dream.html
            (Public_pages.search_results_page ?user ~admin_usernames:[]
               ~rail_communities [] 1 active_tab "" [] [] [] [] request))
    else
      Dream.html
        (Public_pages.search_results_page ?user ~admin_usernames:[] [] 1
           active_tab "" [] [] [] [] request)
  else begin
    let limit = Public_pagination.page_size in
    let offset = Public_pagination.offset page in
    (* The page renders only the active tab, so only that tab's query runs;
       the other three answer empty without touching the database. Unknown
       tab values render as the threads tab and query as it. *)
    let tab =
      match active_tab with
      | "communities" -> `Communities
      | "people" -> `People
      | "comments" -> `Comments
      | _ -> `Posts
    in
    let only wanted run = if tab = wanted then run () else Lwt.return_ok [] in

    Dream.sql request (fun db ->
        let%lwt communities_res =
          only `Communities (fun () ->
              Community_store.search_communities db search_term limit offset)
        in
        let%lwt users_res =
          only `People (fun () ->
              User_store.search_users db search_term limit offset)
        in
        let%lwt posts_res =
          only `Posts (fun () ->
              Post_store.search_posts db search_term limit offset)
        in
        let%lwt comments_res =
          only `Comments (fun () ->
              Comment_store.search_comments db search_term limit offset)
        in
        let%lwt user_votes =
          if user_id > 0 then User_store.get_user_post_votes db user_id
          else Lwt.return_ok []
        in
        let%lwt admin_usernames_res = User_store.get_admin_usernames db in
        let admin_usernames =
          match admin_usernames_res with Ok l -> l | Error _ -> []
        in
        (* Joined communities feed the launch rail only; a failure degrades to
           an empty rail rather than failing the search. *)
        let%lwt rail_communities =
          if user_id > 0 then
            match%lwt Membership_store.get_user_communities db user_id with
            | Ok cs -> Lwt.return cs
            | Error _ -> Lwt.return []
          else Lwt.return []
        in

        match
          (communities_res, users_res, posts_res, comments_res, user_votes)
        with
        | Ok communities, Ok users, Ok posts, Ok comments, Ok votes ->
            (* Chat-provenance is only shown on the Threads tab, so fetch it only there — one
               bounded query over this page's post ids (no N+1). A failure degrades to no
               markers rather than failing the whole search. *)
            let%lwt chat_sources =
              if
                active_tab = "communities" || active_tab = "people"
                || active_tab = "comments"
              then Lwt.return []
              else
                match%lwt
                  Thread_source_store.get_thread_sources_for_posts db
                    (List.map (fun (p : Post_types.post) -> p.id) posts)
                with
                | Ok l -> Lwt.return l
                | Error _ -> Lwt.return []
            in
            Dream.html
              (Public_pages.search_results_page ?user ~admin_usernames
                 ~chat_sources ~rail_communities votes page active_tab
                 search_term communities users posts comments request)
        | _ ->
            Dream.respond ~status:`Internal_Server_Error
              (Site_pages.msg_page ?user ~title:"Error"
                 ~message:"Database error during search. Please try again."
                 ~alert_type:"error" ~return_url:"/" request))
  end

(* The empty-query prompt never reads a page number; with a query, the page
   is checked before any database work. *)
let search_handler request =
  let search_term =
    match Dream.query request "q" with Some q -> String.trim q | None -> ""
  in
  match Public_pagination.parse (Dream.query request "page") with
  | Error `Out_of_range when search_term <> "" ->
      Public_pagination.out_of_range
        ?user:(Dream.session_field request "username")
        ~return_url:"/search" request
  | Ok page -> search ~page request
  | Error `Out_of_range -> search ~page:1 request

(* GET /api/unread-notifs is gone with the client-side badge it existed to
   feed. It could only answer "0" when the count query failed, which the
   browser could not distinguish from a real zero. The count is now resolved
   server-side, once per request, by Notification_badge.middleware. *)

(* === LEGAL / PRIVACY === *)

let privacy_page_handler request =
  let user = Dream.session_field request "username" in
  Dream.html (Site_pages.privacy_page ?user request)
