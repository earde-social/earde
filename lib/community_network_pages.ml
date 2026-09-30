(* The public Network page of a community (GET /c/:slug/network) — the
   complete lists of connected projects and connected communities, moved off
   the community home when the home was reorganized around participation.

   Pure composition: no Caqti, no read-model dependency, no session access, no
   JavaScript. Both lists arrive as the pre-rendered fragments the community
   page already used (Community_connected_projects_pages /
   Community_connected_communities_pages), spliced verbatim, so this page and
   the home cannot present the same relation differently and no visibility or
   eligibility rule is restated here. A fragment that came back empty is
   replaced by that same module's quiet empty section, because this page names
   both destinations whether or not either holds anything — it is a stable
   navigation target, not a conditional block.

   Everything the private management surface knows — lifecycle status,
   direction, request notes, requester/reviewer/remover identities, connection
   ids — is unrepresentable here: it is absent from both fragment models, so
   none of it can leak through this page either. The only management
   affordance is a link to the existing authorized flow, gated by the caller
   and granting nothing: that surface reauthorizes from scratch. *)

let esc = Components.html_escape

let title_copy = "Network"
let intro_copy = "Projects and communities connected to this community."
let connect_copy = "Connect a community"

(* Each list gets its own anchor so the home's two compact rows can land on
   the block they name, without a second page or a second route. *)
let block ~anchor ~fragment ~empty =
  Printf.sprintf "<div class='cnet-block' id='%s'>%s</div>" anchor
    (if fragment = "" then empty else fragment)

let community_network_page ?user ?(noindex = false) ?(rail_communities = [])
    ~(community : Community_types.community) ~sidebar ~projects_section
    ~communities_section ~can_connect request =
  let slug = esc community.slug in
  (* The connect flow is the community's own existing route; the link is a
     shortcut for an authorized viewer, never an authorization. *)
  let cta =
    if can_connect then
      Printf.sprintf
        "<div class='cnet-cta'><a class='btn btn--secondary btn--sm' \
         href='/c/%s/settings/connections/new'>%s</a></div>"
        slug connect_copy
    else ""
  in
  let head =
    Printf.sprintf
      "<div class='chead'><div class='chead__row'><div class='launch-chead-id'>\
       <div class='titleline'><h1 class='chead__title'>%s</h1><span \
       class='chead__slug'>/c/%s</span></div>\
       <p class='chead__desc'>%s</p>%s</div></div></div>"
      title_copy slug intro_copy cta
  in
  let content =
    Printf.sprintf
      "<div class='scroll'>%s<div class='container cnet-body'><div \
       class='stack'>%s%s</div></div></div>"
      head
      (block ~anchor:"projects" ~fragment:projects_section
         ~empty:Community_connected_projects_pages.empty_projects_section)
      (block ~anchor:"communities" ~fragment:communities_section
         ~empty:Community_connected_communities_pages
                .empty_communities_section)
  in
  Community_shell.launch_community_page ?user ~noindex ~request ~rail_communities
    ~community ~sidebar ~page_class:"launch-community-network"
    ~title:(community.name ^ " — Network")
    ~content ()
