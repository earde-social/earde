let new_community_page request =
  (* Legacy generic creation is global-admin only; everyone else lands on the
     onboarding explainer. Admins keep the pre-existing flow untouched. The
     decision rule is unchanged — only its input is now CURRENT durable
     authority, with an unanswerable lookup landing on the same explainer
     rather than opening the form. *)
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ -> Dream.redirect request "/bring"
  | admin_state ->
  match
    Project_onboarding.legacy_creation_get_decision
      ~is_admin:(admin_state = Current_admin)
  with
  | Project_onboarding.Redirect_to_bring | Project_onboarding.Forbid ->
      Dream.redirect request "/bring"
  | Project_onboarding.Show_form ->
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some uid_str ->
      let user = Dream.session_field request "username" in
      (* Rail data only AFTER both gates above: denied or redirected viewers
         never reach this query. Best-effort — a failure degrades to an
         empty rail rather than blocking the admin utility. *)
      let%lwt rail_communities =
        match int_of_string_opt uid_str with
        | None -> Lwt.return []
        | Some uid ->
            Dream.sql request (fun db ->
                match%lwt Membership_store.get_user_communities db uid with
                | Ok communities -> Lwt.return communities
                | Error _ -> Lwt.return [])
      in
      Dream.html (Community_settings_pages.new_community_form ?user ~rail_communities request)

let create_community_handler request =
  (* Server-side admin gate before any form parsing, so a forged form from a
     non-admin session (or no session) is rejected outright. The authority is
     CURRENT durable users.is_admin; an unanswerable lookup creates nothing. *)
  match%lwt Admin_authority.current_admin_of_request request with
  | Current_admin_storage_error _ ->
      Dream.respond ~status:`Internal_Server_Error Handler_support.generic_db_error
  | admin_state ->
  match
    Project_onboarding.legacy_creation_post_decision
      ~is_admin:(admin_state = Current_admin)
  with
  | Project_onboarding.Forbid | Project_onboarding.Redirect_to_bring ->
      Dream.respond ~status:`Forbidden
        "Forbidden: community creation is restricted to Earde administrators."
  | Project_onboarding.Show_form ->
  match Dream.session_field request "user_id" with
  | None ->
      Dream.redirect request "/login"
  | Some uid_str ->
      let user_id = int_of_string uid_str in
      match%lwt Dream.form request with
      | `Ok form_data ->
          let name = String.trim (List.assoc_opt "name" form_data |> Option.value ~default:"") in
          let slug = String.trim (List.assoc_opt "slug" form_data |> Option.value ~default:"") in

          let description_str = List.assoc_opt "description" form_data |> Option.value ~default:"" in
          let description =
            if description_str = "" then None else Some description_str
          in

          (* Post-pivot: every community is a structured shell. The form's
             community_type field is ignored — no community is ever "simple". *)
          let sections_enabled = true in

          let section_count = List.assoc_opt "section_count" form_data |> Option.value ~default:"0" |> int_of_string in

          (* Validate before hitting DB — slug uniqueness error is more helpful than a generic 500. *)
          if name = "" || slug = "" then
            Dream.html (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Validation Error" ~message:"Community name and URL slug are required." ~alert_type:"error" ~return_url:"/new-community" request)
          else

          Analytics_handlers.with_analytics_after_sql (fun record ->
          Dream.sql request (fun db ->
            match%lwt Community_store.create_community db name slug description sections_enabled with
            | Ok () ->
                (* Divine Right: creator becomes first top_mod automatically.
                   Round-trip via get_community_by_slug is necessary — INSERT does not
                   return the new id, and changing create_community's return type would
                   cascade through the mli and all other callers.
                   add_top_moderator (not add_moderator) so the creator can see the
                   Manage Moderators link immediately — default role is 'mod'. *)
                (match%lwt Community_store.get_community_by_slug db slug with
                 | Ok (Some community) ->
                     (* Shell invariant: every community must have a default General forum
                        section and a default general chat channel. These two are created
                        FIRST and their results are CHECKED — a community is only considered
                        successfully created if both exist, so the invariant is real rather
                        than aspirational. (top-mod/join/custom-sections below stay
                        best-effort, matching prior behaviour.) *)
                     let setup_error () =
                       Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Could not finish setting up the community. Please try again." ~alert_type:"error" ~return_url:"/new-community" request)
                     in
                     (match%lwt Section_store.create_section db community.id "General" (Some "General discussion") 0 "new" false with
                      | Error _ -> setup_error ()
                      | Ok () ->
                          (match%lwt Channel_store.create_channel db community.id "general" (Some "General chat") 0 with
                           | Error _ -> setup_error ()
                           | Ok _slug ->
                               let%lwt _ = Moderator_store.add_top_moderator db user_id community.id in
                               let%lwt _ = Membership_store.join_community db user_id community.id in
                               (* Insert any custom sections from the form, in addition to General. *)
                               let%lwt () = Lwt_list.iteri_s (fun i pos ->
                                 let idx = i + 1 in
                                 let sname = String.trim (List.assoc_opt ("section_name_" ^ string_of_int idx) form_data |> Option.value ~default:"") in
                                 if sname = "" then Lwt.return_unit
                                 else begin
                                   let sdesc_str = List.assoc_opt ("section_desc_" ^ string_of_int idx) form_data |> Option.value ~default:"" in
                                   let sdesc = if sdesc_str = "" then None else Some sdesc_str in
                                   let ssort = List.assoc_opt ("section_sort_" ^ string_of_int idx) form_data |> Option.value ~default:"new" in
                                   let%lwt _ = Section_store.create_section db community.id sname sdesc pos ssort false in
                                   Lwt.return_unit
                                 end
                               ) (List.init section_count (fun i -> i + 1)) in
                               (* Insert any optional extra live-chat channels, in addition to
                                  the default #general. Best-effort like custom sections (errors
                                  ignored, no transaction). channel_count is clamped to a small
                                  max so a hand-crafted form can't force a huge insert loop;
                                  Channel_store.create_channel slugifies + dedupes, so blank/duplicate names
                                  can't violate the UNIQUE (community_id, slug) constraint. The
                                  default general channel sits at position 0, so extras start at 1. *)
                               let channel_count =
                                 List.assoc_opt "channel_count" form_data
                                 |> Option.value ~default:"0"
                                 |> (fun s -> match int_of_string_opt s with Some n -> n | None -> 0)
                                 |> max 0 |> min 20
                               in
                               let%lwt () = Lwt_list.iteri_s (fun i _ ->
                                 let idx = i + 1 in
                                 let cname = String.trim (List.assoc_opt ("channel_name_" ^ string_of_int idx) form_data |> Option.value ~default:"") in
                                 (* skip blanks and an explicit "general" so we don't shadow the
                                    default #general with a "general-2". Any odd input that still
                                    slugifies to general is harmless — create_channel just dedupes. *)
                                 if cname = "" || String.lowercase_ascii cname = "general" then Lwt.return_unit
                                 else begin
                                   let%lwt _ = Channel_store.create_channel db community.id cname None idx in
                                   Lwt.return_unit
                                 end
                               ) (List.init channel_count (fun i -> i + 1)) in
                               (* §5.3: creation emits only $groupidentify (no
                                  community_created event), attributed to the
                                  creator's authenticated identity, from the
                                  round-tripped authoritative record. *)
                               record (fun () ->
                                   Analytics.identify_community_if_consented request
                                     ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
                                     (Analytics_handlers.community_group_of community));
                               Dream.redirect request ("/c/" ^ slug)))
                 | _ -> Dream.redirect request ("/c/" ^ slug))
            | Error _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Error" ~message:"Could not create community. The URL slug may already be taken." ~alert_type:"error" ~return_url:"/new-community" request)
          ))
      | _ -> Dream.respond ~status:`Bad_Request (Site_pages.msg_page ?user:(Dream.session_field request "username") ~title:"Form Error" ~message:"Your form submission was invalid. Please try again." ~alert_type:"error" ~return_url:"/new-community" request)

(* === Connected projects on the community page ===
   The accepted project-home relations of a community, rendered inside the community page
   the route already serves. Deliberately read only AFTER the route's own community lookup
   and can_view_community decision have completed, so this section can never become a side
   channel that reveals a private, draft, or otherwise unviewable community — it only adds
   detail to a page the viewer was already entitled to see. It broadens access to nothing:
   no GitHub authorization is consulted, and anonymous visitors to a public community see
   exactly what any other viewer of that page sees. *)

(* The read model and the page module are deliberately independent — neither depends on the
   other — so this route is the one place the two vocabularies meet. *)
let connected_project_page_model project =
  let module R = Community_connected_projects_read_model in
  let repositories =
    List.map
      (fun r : Community_connected_projects_pages.repository ->
        { full_name = R.repository_full_name r;
          html_url = R.repository_html_url r;
          is_primary = R.repository_is_primary r;
          is_archived = R.repository_is_archived r })
      (R.project_repositories project)
  in
  let verification : Community_connected_projects_pages.verification =
    match R.project_verification project with
    | R.Verified -> Verified
    | R.Stale -> Stale
    | R.Revoked -> Revoked
  in
  ({ name = R.project_name project;
     slug = R.project_slug project;
     kind = R.project_kind project;
     namespace_login = R.project_namespace_login project;
     verification;
     website_url = R.project_website_url project;
     repositories }
    : Community_connected_projects_pages.project)

(* One generic, non-cacheable 500: no Caqti/PostgreSQL detail, error constructor, or durable
   value reaches the page. Durable corruption is never rendered away as a quietly incomplete
   community page. *)
let connected_projects_error_page ?user request =
  Dream.respond ~status:`Internal_Server_Error
    ~headers:[ ("Cache-Control", "no-store"); ("Pragma", "no-cache") ]
    (Site_pages.msg_page ?user ~title:"Error"
       ~message:"Something went wrong on our side. Please try again."
       ~alert_type:"error" ~return_url:"/" request)

(* A slug or community that no longer resolves reuses the route's existing generic
   unavailable response, byte-for-byte — a community that vanished between the route's own
   lookup and this read must not become distinguishable from one that never existed.

   The continuation receives the page models, not a rendered fragment: three surfaces now
   read the same publicly visible set and need it differently — the community page renders
   the full block, the community home shows only how many there are, and the Network page
   renders the full block again. Rendering at the call site keeps that one read, and one
   visibility rule, shared. *)
let with_connected_projects db ?user request ~community_slug k =
  let module R = Community_connected_projects_read_model in
  match%lwt R.load_for_community db ~community_slug with
  | Ok projects -> k (List.map connected_project_page_model projects)
  | Error (R.Invalid_community_slug | R.Community_unavailable) ->
      Community_read_gate.community_not_found ?user request
  | Error (R.Inconsistent_data | R.Storage_error) ->
      connected_projects_error_page ?user request

(* The community↔community counterpart of [with_connected_projects], on the same discipline:
   read only AFTER the route's own community lookup and can_view_community decision, reuse the
   route's generic unavailable response for a slug that stopped resolving, and never render
   durable corruption away as a quietly incomplete page.

   Public visibility is entirely the read model's: it applies the connection-eligibility
   predicate to the viewed community and to every counterpart, so this helper has no rule of
   its own to keep in sync. An ineligible community simply comes back with an empty list and
   the fragment collapses to "". *)
let with_connected_communities db ?user request ~community_slug k =
  let module R = Community_connected_communities_read_model in
  match%lwt R.load_for_community db ~community_slug with
  | Ok communities ->
      k
        (List.map
           (fun c : Community_connected_communities_pages.connected_community ->
             { name = R.community_name c; slug = R.community_slug c })
           communities)
  | Error (R.Invalid_community_slug | R.Community_unavailable) ->
      Community_read_gate.community_not_found ?user request
  | Error (R.Inconsistent_data | R.Storage_error) ->
      connected_projects_error_page ?user request

(* The private settings counterpart of [with_connected_projects]: the same read model, the
   same generic failure responses, but rendered through the removal-pages management
   fragment so each accepted project carries a removal form.

   [authorized] is the settings surface's own top-mod/admin decision, and it gates the read
   itself — an ordinary moderator's settings page never issues this query and never receives
   a fragment, so the panel cannot exist for them. Rendering a form still authorizes nothing:
   Project_home_removal_store reauthorizes every removal POST against the three durable
   sources.

   [removal_allowed] is this surface's own reading of the community it already
   loaded: an unpublished network setup draft structurally requires its
   provisioned home, so the section renders identities without controls. It
   suppresses a form, never a fact, and the store stays authoritative — a forged
   POST against a protected draft is refused there, not here. *)
let with_settings_connected_projects db ?user request ~community_slug ~authorized
    ~removal_allowed k =
  let module R = Community_connected_projects_read_model in
  if not authorized then k Html.empty
  else
    match%lwt R.load_for_community db ~community_slug with
    | Ok projects ->
        let page_model project =
          let verification : Project_home_removal_pages.verification =
            match R.project_verification project with
            | R.Verified -> Verified
            | R.Stale -> Stale
            | R.Revoked -> Revoked
          in
          ({ name = R.project_name project;
             slug = R.project_slug project;
             namespace_login = R.project_namespace_login project;
             verification }
            : Project_home_removal_pages.connected_project)
        in
        k
          (Project_home_removal_pages.community_side_management_section ~request
             ~removal_allowed ~community_slug
             ~projects:(List.map page_model projects) ())
    | Error (R.Invalid_community_slug | R.Community_unavailable) ->
        Community_read_gate.community_not_found ?user request
    | Error (R.Inconsistent_data | R.Storage_error) ->
        (* Durable corruption is never rendered away as a quietly incomplete settings
           page. *)
        connected_projects_error_page ?user request

let community_page_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let sort_str_opt = Dream.query request "sort" in
  let page = match Dream.query request "page" with Some p -> (try int_of_string p with _ -> 1) | None -> 1 in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  let shared_sidebar_data db (community : Community_types.community) =
    let%lwt mods_res = Moderator_store.get_community_mods_with_roles db community.id in
    let%lwt admin_usernames_res = User_store.get_admin_usernames db in
    let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
    let%lwt banned_res = Community_ban_store.get_banned_users db community.id in
    let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: User_store.user) -> u.username) bs | _ -> [] in
    let%lwt user_communities_res = if user_id > 0 then Membership_store.get_user_communities db user_id else Lwt.return_ok [] in
    let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
    let%lwt moderated_communities_res = if user_id > 0 then Moderator_store.get_moderated_communities db user_id else Lwt.return_ok [] in
    let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
    let%lwt is_mem = if user_id > 0 then Membership_store.is_member db user_id community.id else Lwt.return_ok false in
    Lwt.return (mods_res, admin_usernames, banned_usernames, user_communities, moderated_communities, is_mem)
  in

  Dream.sql request (fun db ->
    (* CURRENT durable admin authority, resolved once for both branches' read
       gates. A viewer whose session does not claim admin costs no query. *)
    let%lwt is_admin = Admin_authority.current_admin_read_override db request in
    let%lwt _ = Moderator_store.demote_inactive_mods db in
    match%lwt Community_store.get_community_by_slug db slug with
    | Ok (Some community) when community.sections_enabled ->
        let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
        if not authorized then Community_read_gate.community_not_found ?user request
        else
        (* Structured community: /c/:slug always shows the sections overview.
           Section feeds are at /c/:slug/s/:section_slug. *)
        let%lwt sections_res = Section_store.get_sections_with_stats db community.id in
        let%lwt orphaned_res = Section_store.get_orphaned_count_and_activity db community.id in
        (* One clean call each for the channels and recent-discussions blocks. Both degrade to
           an empty list on error so the overview still renders — neither is load-bearing. *)
        let%lwt channels_res = Channel_store.get_channels_by_community db community.id in
        let channels = match channels_res with
          | Ok cs -> List.filter (fun (c : Channel_store.channel) -> not c.is_archived) cs
          | Error _ -> []
        in
        let%lwt recent_posts_res = Post_store.get_posts_by_community db community.id Post_types.Newest 5 0 in
        let recent_posts = match recent_posts_res with Ok ps -> ps | Error _ -> [] in
        let%lwt (mods_res, _admin_usernames, _banned_usernames, user_communities, _moderated_communities, is_mem) =
          shared_sidebar_data db community
        in
        (match sections_res, is_mem with
         | Ok section_stats, Ok m ->
             let orphaned = match orphaned_res with Ok o -> o | Error _ -> (0, None) in
             let mods = match mods_res with Ok ms -> ms | _ -> [] in
             let mod_usernames = List.map (fun (e: Moderator_store.moderator_entry) -> e.username) mods in
             let is_mod = user_id > 0 && List.exists (fun (e: Moderator_store.moderator_entry) -> e.user_id = user_id) mods in
             let is_top_mod = user_id > 0 && List.exists (fun (e: Moderator_store.moderator_entry) -> e.user_id = user_id && e.role = "top_mod") mods in
             (* Order: lookup → authorization → existing page data → connected projects →
                connected communities → render. Never before the authorization decision
                above. The home shows only how many of each are publicly visible and
                links to /c/:slug/network for the lists themselves, so it takes the
                counts of exactly the sets that page renders — one read model, one
                visibility rule, two presentations. *)
             with_connected_projects db ?user request ~community_slug:slug (fun projects ->
             with_connected_communities db ?user request ~community_slug:slug (fun communities ->
               Dream.html (Community_pages.community_overview_page ?user ~noindex:(Community_read_gate.community_noindex community) ~connected_projects_count:(List.length projects) ~connected_communities_count:(List.length communities) ~is_member:m ~is_current_user_mod:is_mod ~is_current_user_top_mod:is_top_mod ~mod_usernames ~orphaned ~rail_communities:user_communities ~channels ~recent_posts community section_stats request)))
         | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:"Failed to load community sections." ~alert_type:"error" ~return_url:"/" request))
    | Ok (Some community) ->
        let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
        if not authorized then Community_read_gate.community_not_found ?user request
        else
        (* Simple community feed *)
        let sort_mode = match sort_str_opt with
          | Some "new" -> Post_types.Newest | Some "top" -> Post_types.Top | Some "hot" -> Post_types.Hot | _ -> Post_types.Hot
        in
        let sort_str = match sort_mode with Post_types.Newest -> "new" | Post_types.Top -> "top" | Post_types.Hot -> "hot" | Post_types.Active -> "active" in
        let%lwt posts = Post_store.get_posts_by_community db community.id sort_mode limit offset in
        let%lwt user_votes = if user_id > 0 then User_store.get_user_post_votes db user_id else Lwt.return_ok [] in
        let%lwt (mods_res, admin_usernames, banned_usernames, user_communities, moderated_communities, is_mem) =
          shared_sidebar_data db community
        in
        (match posts, user_votes, is_mem with
         | Ok p, Ok v, Ok m ->
             let mods = match mods_res with Ok ms -> ms | _ -> [] in
             let mod_usernames = List.map (fun (e: Moderator_store.moderator_entry) -> e.username) mods in
             let is_mod = user_id > 0 && List.exists (fun (e: Moderator_store.moderator_entry) -> e.user_id = user_id) mods in
             let is_top_mod = user_id > 0 && List.exists (fun (e: Moderator_store.moderator_entry) -> e.user_id = user_id && e.role = "top_mod") mods in
             (* Same order as the structured branch: both connected-* reads follow the
                authorization decision and the existing feed load. The flat community
                page keeps both full blocks in its side stack — the home reorganization
                is the structured overview's. *)
             with_connected_projects db ?user request ~community_slug:slug (fun projects ->
             let connected_projects =
               Community_connected_projects_pages.connected_projects_section ~projects
             in
             with_connected_communities db ?user request ~community_slug:slug (fun communities ->
             let connected_communities =
               Community_connected_communities_pages.connected_communities_section
                 ~communities
             in
               Dream.html (Community_pages.community_page ?user ~noindex:(Community_read_gate.community_noindex community) ~connected_projects ~connected_communities ~is_member:m ~is_current_user_mod:is_mod ~is_current_user_top_mod:is_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities v page sort_str community p request)))
         | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:"Failed to load community data." ~alert_type:"error" ~return_url:"/" request))
    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* === Public Network page: GET /c/:slug/network ===
   The community's external network — the complete connected-projects and connected-
   communities lists that used to occupy the community home's main column. The home now
   carries only a compact entry point with the two counts, so this is where the lists
   themselves live.

   Same access discipline as the sibling public community routes and deliberately no new
   one: the community is resolved, can_view_community decides, and only then is anything
   else read. Both lists come from the same two helpers the community page uses, so the
   eligibility and visibility rules are the read models' single copy — this route restates
   none of them and can reveal nothing /c/:slug would not.

   The Connect-a-community link is gated by the same top-mod-or-admin reading the community
   sidebar already applies to that destination. It authorizes nothing: the connections
   surface re-decides every request in SQL. *)
let community_network_handler request =
  let slug = Dream.param request "slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with
    | Some id -> (try int_of_string id with _ -> 0) | None -> 0 in
  Dream.sql request (fun db ->
    (* CURRENT durable admin authority: the read gate below, and the two
       management affordances further down, all take this value rather than
       the session's cached claim. *)
    let%lwt is_admin = Admin_authority.current_admin_read_override db request in
    match%lwt Community_store.get_community_by_slug db slug with
    | Ok (Some community) ->
        let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
        if not authorized then Community_read_gate.community_not_found ?user request
        else
        (* Launch-chrome data, loaded only after the authorization decision above:
           sections/channels feed the shared community sidebar and the viewer's joined
           communities the global rail. Each degrades to an empty list rather than
           blocking the page — none of it is load-bearing. *)
        let%lwt sections =
          if community.sections_enabled then
            (match%lwt Section_store.get_sections_by_community db community.id with
             | Ok secs -> Lwt.return secs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        let%lwt channels =
          match%lwt Channel_store.get_channels_by_community db community.id with
          | Ok cs -> Lwt.return cs | Error _ -> Lwt.return []
        in
        let%lwt rail_communities =
          if user_id > 0 then
            (match%lwt Membership_store.get_user_communities db user_id with
             | Ok cs -> Lwt.return cs | Error _ -> Lwt.return [])
          else Lwt.return []
        in
        let%lwt mods_res = Moderator_store.get_community_mods_with_roles db community.id in
        let mods = match mods_res with Ok ms -> ms | Error _ -> [] in
        let is_mod =
          user_id > 0 && List.exists (fun (e : Moderator_store.moderator_entry) -> e.user_id = user_id) mods in
        let is_top_mod =
          user_id > 0
          && List.exists
               (fun (e : Moderator_store.moderator_entry) -> e.user_id = user_id && e.role = "top_mod")
               mods
        in
        let sidebar =
          Community_pages.launch_knowledge_sidebar ~community ~channels ~sections
            ~can_manage:(is_mod || is_admin) ()
        in
        with_connected_projects db ?user request ~community_slug:slug (fun projects ->
        with_connected_communities db ?user request ~community_slug:slug (fun communities ->
          Dream.html
            (Community_network_pages.community_network_page ?user
               ~noindex:(Community_read_gate.community_noindex community) ~rail_communities ~community ~sidebar
               ~projects_section:
                 (Community_connected_projects_pages.connected_projects_section ~projects)
               ~communities_section:
                 (Community_connected_communities_pages.connected_communities_section
                    ~communities)
               ~can_connect:(is_top_mod || is_admin) request)))
    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error err -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
  )

(* Section feed at /c/:slug/s/:section_slug — pretty URL replaces the old ?section=id param. *)
let community_section_handler request =
  let community_slug = Dream.param request "slug" in
  let section_slug = Dream.param request "section_slug" in
  let user = Dream.session_field request "username" in
  let user_id = match Dream.session_field request "user_id" with Some id -> int_of_string id | None -> 0 in
  let sort_str_opt = Dream.query request "sort" in
  let page = match Dream.query request "page" with Some p -> (try int_of_string p with _ -> 1) | None -> 1 in
  let limit = 20 in
  let offset = (max 1 page - 1) * limit in

  Dream.sql request (fun db ->
    (* CURRENT durable admin authority for the private-community read gate. *)
    let%lwt is_admin = Admin_authority.current_admin_read_override db request in
    let%lwt _ = Moderator_store.demote_inactive_mods db in
    match%lwt Community_store.get_community_by_slug db community_slug with
    | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This community does not exist." ~alert_type:"error" ~return_url:"/" request)
    | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
    | Ok (Some community) ->
        let%lwt authorized = Community_read_gate.can_view_community db ~user_id ~admin_override:is_admin community in
        if not authorized then Community_read_gate.community_not_found ?user request
        else
        let render_section_feed (section : Section_store.community_section) sort_mode sort_str fetch_posts =
          let%lwt posts = fetch_posts () in
          let%lwt user_votes = if user_id > 0 then User_store.get_user_post_votes db user_id else Lwt.return_ok [] in
          let%lwt mods_res = Moderator_store.get_community_mods_with_roles db community.id in
          let%lwt admin_usernames_res = User_store.get_admin_usernames db in
          let admin_usernames = match admin_usernames_res with Ok l -> l | Error _ -> [] in
          let%lwt banned_res = Community_ban_store.get_banned_users db community.id in
          let banned_usernames = match banned_res with Ok bs -> List.map (fun (u: User_store.user) -> u.username) bs | _ -> [] in
          let%lwt user_communities_res = if user_id > 0 then Membership_store.get_user_communities db user_id else Lwt.return_ok [] in
          let user_communities = match user_communities_res with Ok us -> us | _ -> [] in
          let%lwt moderated_communities_res = if user_id > 0 then Moderator_store.get_moderated_communities db user_id else Lwt.return_ok [] in
          let moderated_communities = match moderated_communities_res with Ok l -> l | Error _ -> [] in
          let%lwt is_mem = if user_id > 0 then Membership_store.is_member db user_id community.id else Lwt.return_ok false in
          (* One stats query drives BOTH the col-2 sidebar and the right-rail Threads/Last-activity
             rows — same ordering as get_sections_by_community, so the sidebar is unchanged, and no
             fabricated numbers. Folded into the success match below so a DB error surfaces as the
             standard error page, never an empty sidebar. *)
          let%lwt sections_res = Section_store.get_sections_with_stats db community.id in
          (* Channels feed the shell's new Channels nav group; a load error degrades to an
             empty group rather than failing the whole section page. *)
          let%lwt channels_res = Channel_store.get_channels_by_community db community.id in
          let channels = match channels_res with Ok cs -> cs | Error _ -> [] in
          (match posts, user_votes, is_mem, sections_res with
           | Ok p, Ok v, Ok _m, Ok section_stats ->
               let mods = match mods_res with Ok ms -> ms | _ -> [] in
               let mod_usernames = List.map (fun (e: Moderator_store.moderator_entry) -> e.username) mods in
               let is_mod = user_id > 0 && List.exists (fun (e: Moderator_store.moderator_entry) -> e.user_id = user_id) mods in
               let _ = sort_mode in
               let _ = moderated_communities in  (* unused by the shell page; kept loaded above for parity *)
               let sections = List.map (fun ((s : Section_store.community_section), _, _) -> s) section_stats in
               (* Real Threads/Last-activity for the rail: find this section in the stats. The virtual
                  Uncategorized feed isn't a community_sections row, so fall back to the orphaned-posts
                  aggregate. Any miss → None → the rail just omits that row (never faked). *)
               let%lwt (thread_count, last_activity) =
                 match List.find_opt (fun ((s : Section_store.community_section), _, _) -> s.section_id = section.Section_store.section_id) section_stats with
                 | Some (_, cnt, act) -> Lwt.return (Some cnt, act)
                 | None when section.slug = "uncategorized" ->
                     (match%lwt Section_store.get_orphaned_count_and_activity db community.id with
                      | Ok (cnt, act) -> Lwt.return (Some cnt, act)
                      | Error _ -> Lwt.return (None, None))
                 | None -> Lwt.return (None, None)
               in
               (* rail shows the communities the user belongs to; the shell wraps layout itself. *)
               Dream.html (Community_pages.community_section_shell_page ?user ~noindex:(Community_read_gate.child_noindex community ~child_indexable:section.indexable) ?thread_count ?last_activity ~is_current_user_mod:is_mod ~mod_usernames ~admin_usernames ~banned_usernames ~rail_communities:user_communities ~channels ~sections ~section ~user_votes:v ~current_page:page ~sort_mode:sort_str ~community ~posts:p request)
           | _ -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:"Failed to load section." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request))
        in
        if section_slug = "uncategorized" then begin
          (* Virtual section: shows posts orphaned by deleted sections. *)
          let sort_mode =
            match sort_str_opt with
            | Some "top" -> Post_types.Top | Some "hot" -> Post_types.Hot | Some "active" -> Post_types.Active
            | _ -> Post_types.Newest
          in
          let sort_str = match sort_mode with Post_types.Newest -> "new" | Post_types.Top -> "top" | Post_types.Hot -> "hot" | Post_types.Active -> "active" in
          let%lwt orphaned_count_res = Section_store.get_orphaned_count_and_activity db community.id in
          let orphaned_count = match orphaned_count_res with Ok (c, _) -> c | Error _ -> 0 in
          if orphaned_count = 0 then
            Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"No uncategorized posts in this community." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
          else
            let virtual_section = {
              Section_store.section_id = -1; community_id = community.id;
              name = "Uncategorized"; slug = "uncategorized";
              description = Some "Posts from deleted sections";
              position = 9999; default_sort = "new"; is_introduction_section = false;
              indexable = true;
            } in
            render_section_feed virtual_section sort_mode sort_str
              (fun () -> Section_store.get_orphaned_posts db community.id sort_mode limit offset)
        end else begin
          match%lwt Section_store.get_section_by_slug db section_slug community.id with
          | Ok None -> Dream.respond ~status:`Not_Found (Site_pages.msg_page ?user ~title:"Not Found" ~message:"This section does not exist." ~alert_type:"error" ~return_url:("/c/" ^ community_slug) request)
          | Error e -> Dream.respond ~status:`Internal_Server_Error (Site_pages.msg_page ?user ~title:"Error" ~message:(Handler_support.db_error_message e) ~alert_type:"error" ~return_url:"/" request)
          | Ok (Some section) ->
              let sort_mode =
                let from_query = match sort_str_opt with
                  | Some "new" -> Some Post_types.Newest | Some "top" -> Some Post_types.Top
                  | Some "active" -> Some Post_types.Active | Some "hot" -> Some Post_types.Hot | _ -> None
                in
                match from_query with
                | Some sm -> sm
                | None -> (match section.Section_store.default_sort with
                    | "new" -> Post_types.Newest | "top" -> Post_types.Top | "active" -> Post_types.Active | _ -> Post_types.Hot)
              in
              let sort_str = match sort_mode with Post_types.Newest -> "new" | Post_types.Top -> "top" | Post_types.Hot -> "hot" | Post_types.Active -> "active" in
              render_section_feed section sort_mode sort_str
                (fun () -> Post_store.get_posts_by_section db community.id section.Section_store.section_id sort_mode limit offset)
        end
  )
