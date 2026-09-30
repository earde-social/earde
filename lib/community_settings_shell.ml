(* The one Cartographic Civic community-settings shell: the shared settings
   header band, the grouped internal settings navigation column, and the
   panel wrapper every authorized settings/management surface renders inside.
   Before this module each surface hand-rolled its own variant — the settings
   hub had the only index column, and the Connections / Shared threads /
   Project home requests / Manage moderators / Reports surfaces rendered bare
   panels with drifting headers — so the shell markup lives here once and the
   pages only choose the active item.

   Rendering only: nothing here reads a session, issues SQL, or decides
   authority. Callers pass the authority booleans their route already proved
   (the store/read-model layers keep reauthorizing every mutation), and the
   nav strictly narrows what it shows to what the viewer can open — a link
   grants nothing, but a viewer must never see an entry they categorically
   cannot open. *)

(* Closed vocabulary of internal settings destinations. Exactly one is active
   per rendered surface; query-panel destinations and dedicated-route
   destinations share the one type so no surface can highlight two. *)
type item =
  | Setup_publish
  | Profile
  | Visibility
  | Connected_projects
  | Home_requests
  | Connections
  | Shared_threads
  | Channels
  | Members
  | Manage_moderators
  | Moderation
  | Bans

(* The scoped network-slug grammar the database enforces on every network
   row (moved here from the settings page so every shell surface gates the
   setup link identically). Defensive: a slug outside it never becomes a
   setup link. *)
let canonical_network_slug value =
  let n = String.length value in
  let is_slug_char c = (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') in
  n >= 1 && n <= 80
  && value.[0] <> '-'
  && value.[n - 1] <> '-'
  &&
  let rec ok i =
    i >= n
    ||
    if is_slug_char value.[i] then ok (i + 1)
    else value.[i] = '-' && value.[i + 1] <> '-' && ok (i + 1)
  in
  ok 0

(* Whether the "Complete setup and publish" affordance is worth showing:
   an unpublished network setup draft, an authorized (top-mod/admin) viewer,
   and a canonical slug. The setup surface independently reauthorizes, so
   this only suppresses a pointless link, never grants anything. *)
let can_complete_setup ~(community : Community_types.community) ~authorized =
  community.Community_types.is_network_community
  && community.Community_types.onboarding_state = Community_types.Community_draft
  && authorized
  && canonical_network_slug community.slug

(* The shared settings page header band: community identity plus the one
   canonical back link. Every shell surface renders this exact band, so the
   header can never drift per surface again. *)
let header ~slug =
  let slug = (Html.text (slug)) in
  (Html.template "<div class='cm-head'>\
     <h1 class='cm-h1'>&#x2699;&#xFE0F; /c/%s <span class='accent'>settings</span></h1>\
     <a href='/c/%s' class='cm-back'>&larr; Back to community</a>\
     </div>"
  [ slug
  ; slug ])

(* The grouped internal settings navigation. Groups render only when they
   have at least one visible entry, so a regular moderator gets a coherent
   subset (no empty "Network" heading) and a top moderator or admin gets the
   complete permitted set in the same order.

   [network_manager] is the surface's own top-mod/admin reading (session
   admin flag or role lookup on the settings hub; SQL-proved authorization on
   the dedicated management routes). It gates the Network group and the
   Manage moderators entry — exactly the entries whose routes refuse
   everyone below top_mod/durable admin. *)
let nav ~slug ~active ~can_complete_setup ~network_manager () =
  let link ?(danger = false) ~key href label =
    let active_cls = if key = active then " cm-index-link--active" else "" in
    let danger_cls = if danger then " cm-index-link--danger" else "" in
    Html.template "<a class='cm-index-link%s%s' href='%s'>%s</a>"
      [ Html.text danger_cls; Html.text active_cls; Html.internal_path href;
        label ]
  in
  let path suffix = Printf.sprintf "/c/%s%s" slug suffix in
  let panel_href key = path ("/settings?panel=" ^ key) in
  let group title links =
    match List.filter (fun l -> not (Html.is_empty l)) links with
    | [] -> Html.empty
    | ls ->
        Html.template "<p class='cm-index-group'>%s</p>%s"
          [ Html.text title; Html.concat ls ]
  in
  let community_group =
    group "Community"
      [ (if can_complete_setup then
           link ~key:Setup_publish (path "/setup")
             (Html.static "Complete setup and publish")
         else Html.empty);
        link ~key:Profile (panel_href "profile") (Html.static "Profile");
        link ~key:Visibility (panel_href "visibility")
          (Html.static "Visibility &amp; discovery")
      ]
  in
  let network_group =
    if not network_manager then Html.empty
    else
      group "Network"
        [ link ~key:Connected_projects (panel_href "projects")
            (Html.static "Connected projects");
          link ~key:Home_requests (path "/project-home-requests")
            (Html.static "Project home requests");
          link ~key:Connections (path "/settings/connections")
            (Html.static "Connections");
          link ~key:Shared_threads (path "/settings/shared-threads")
            (Html.static "Shared threads")
        ]
  in
  let structure_group =
    group "Structure"
      [ link ~key:Channels (panel_href "channels")
          (Html.static "Channels &amp; sections") ]
  in
  let people_group =
    group "People"
      [ link ~key:Members (panel_href "members") (Html.static "Members");
        (if network_manager then
           link ~key:Manage_moderators (path "/manage-mods")
             (Html.static "Manage moderators")
         else Html.empty);
        link ~key:Moderation (panel_href "moderation")
          (Html.static "Moderation");
        link ~danger:true ~key:Bans (panel_href "bans") (Html.static "Bans")
      ]
  in
  Html.template
    "<nav class='cm-index'><div class='cm-index-title'>Settings</div>%s%s%s%s</nav>"
    [ community_group; network_group; structure_group; people_group ]

(* The whole shell around a rendered panel: header band, then the index
   column and the scrolling panel column side by side. The wrapper's
   cm-wrap--settings modifier is the one CSS scope for the shell structure,
   so a surface adopting the shell needs no page-class-specific layout CSS. *)
let wrap ~slug ~active ~can_complete_setup ~network_manager ~panel () =
  (Html.template "<div class='cm-wrap cm-wrap--settings'>%s<div class='cm-cols'>%s<div class='cm-main'>%s</div></div></div>"
  [ (header ~slug)
  ; (nav ~slug ~active ~can_complete_setup ~network_manager ())
  ; panel ])
