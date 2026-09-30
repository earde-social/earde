(* The "Connected communities" block of the existing community pages — a pure
   fragment over a handler-supplied page model, built to the same shape as the
   sibling connected-projects fragment so the community page composes two
   blocks that read as one system.

   Deliberately a fragment rather than a page: the community route already
   owns authorization, chrome, cache and indexing policy, and analytics
   behavior. Whether the block may be rendered at all is the read model's
   eligibility decision, taken before this module ever sees a list.

   The model carries identity only, so no status, direction, note, or actor
   can leak through it. See the .mli. *)

type connected_community = { name : string; slug : string }

(* The same single-path-segment grammar the connection surfaces use. A slug
   that fails it labels the community as inert text instead of becoming a
   link to something else. *)
let valid_slug value =
  String.length value > 0
  && String.for_all
       (fun byte ->
         Char.code byte > 0x20 && Char.code byte <> 0x7f && byte <> '/')
       value

(* Factual, and nothing more: two communities are connected. The subtitle
   states the relationship's only property — that it is mutual — rather than
   implying a kind, a dependency, an endorsement, or a hierarchy. *)
let heading_html =
  Html.static
    "<h2 class='ccc-title'>Connected communities</h2><p \
     class='ccc-sub'>Communities this one is mutually connected with.</p>"

let community_html (c : connected_community) =
  let name = Html.text c.name in
  if valid_slug c.slug then
    Html.template
      "<li class='ccc-community'><a class='ccc-name' href='/c/%s'>%s</a></li>"
      [ Html.text c.slug; name ]
  else
    Html.template "<li class='ccc-community'><p class='ccc-name'>%s</p></li>"
      [ name ]

let connected_communities_section ~communities =
  match communities with
  | [] -> Html.empty
  | communities ->
      Html.template
        "<section class='ccc-section'>%s<ul \
         class='ccc-communities'>%s</ul></section>"
        [ heading_html; Html.concat (List.map community_html communities) ]

(* The counterpart of the projects module's empty section, for the Network
   page, which names both destinations even when one holds nothing. Same
   heading constant as the populated block, so the copy has one source. The
   community page's own contract is unchanged: nothing connected, nothing
   rendered. *)
let empty_communities_section =
  Html.template
    "<section class='ccc-section'>%s<p class='ccc-empty'>No connected \
     communities yet.</p></section>"
    [ heading_html ]
