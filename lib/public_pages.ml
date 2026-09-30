open Html.Infix

(* /feed — the global Feed surface, now the first App route on the launch
   chrome (Components.launch_app_page: earde.css only, no Tailwind).
   Rows still come from render_forum_row with ~show_context — its markup is a
   load-bearing contract (optimistic-vote DOM, replay-masking classes, the ⋯
   moderation menu) — and are skinned onto the approved .thread-row anatomy by
   the "/feed only" integration section at the end of earde.css. Mirrors the
   global home/all contract: per-community mod buttons are NOT shown here
   (only admin/own-post actions via render_forum_row's own logic).

   scope is "following" | "all"; logged-out users are always "all" with no
   toggle. Sort chips cover exactly the four sorts feed_handler parses; the
   handoff's type chips are omitted (no ?type= parameter exists). Empty
   Following renders an intentional empty state that points at public content
   + the founder, never a fake Browse-communities link (no such route). *)
let feed_page ?user ~scope ~sort_mode ~is_logged_in ~admin_usernames
    ~(rail_communities : Community_types.community list) ~user_votes ~current_page
    ?(shared_destinations : (int * (string * string) list) list = [])
    (posts : Post_types.post list) request =

  (* Contact links: reuse the existing founder Telegram link; keep the existing pilot mailto. *)
  let founder_tg = "https://t.me/tolwiz" in
  let mail_pilot    = "mailto:metacirculardispatches@gmail.com?subject=Start%20a%20pilot%20community%20on%20Earde" in

  (* Scope toggle (logged-in only). Each link keeps the current sort. *)
  let scope_tabs =
    if not is_logged_in then Html.empty
    else
      let tab s label =
        let cls = if s = scope then "tab tab--active" else "tab" in
        (Html.template "<a class='%s' href='/feed?scope=%s&sort=%s'>%s</a>"
  [ Html.text cls; Html.text s; Html.text sort_mode; label ])
      in
      (Html.template "<nav class='tabs launch-scope-tabs' aria-label='Feed scope'>%s%s</nav>"
  [ tab "following" (Html.static "Following")
  ; tab "all" (Html.static "All communities") ])
  in

  (* Sort chips — keep the current scope. *)
  let chip mode label =
    let cls = if mode = sort_mode then "chip chip--active" else "chip" in
    (Html.template "<a class='%s' href='/feed?scope=%s&sort=%s'>%s</a>"
  [ Html.text cls; Html.text scope; Html.text mode; label ])
  in
  let sort_bar =
    (Html.template "<div class='tabbar launch-sort-bar'><div class='chips' aria-label='Sort'>%s%s%s%s</div></div>"
  [ chip "hot" (Html.static "Hot")
  ; chip "new" (Html.static "New")
  ; chip "top" (Html.static "Top")
  ; chip "active" (Html.static "Active") ])
  in

  let page_head =
    (Html.template "<div class='page__head'><div class='page__head-inner'>\
       <h1 class='page__title'>Feed</h1>\
       <p class='page__sub'>Live activity and durable knowledge across the communities you follow.</p>\
       %s%s</div></div>"
  [ scope_tabs
  ; sort_bar ])
  in

  (* Empty state. Following-empty keeps its two real destinations (public
     threads + the pilot mailto) under the launch .empty idiom. *)
  let posts_html =
    if posts <> [] then
      (* Origin-side enrichment only: each card is still the one canonical
         row the feed query selected — [shared_destinations] adds a
         provenance span and can neither add, drop, nor reorder cards. *)
      (Html.join (Html.static "\n")) (List.map (fun (p : Post_types.post) ->
        let shared_with =
          Option.value ~default:[] (List.assoc_opt p.id shared_destinations) in
        Post_cards.render_forum_row ~admin_usernames ~show_context:true
          ~shared_with request user_votes p) posts)
    else if scope = "following" && is_logged_in then
      (Html.template "<div class='empty'>\
         <div class='empty__title'>Your feed is empty</div>\
         <p class='empty__body'>You're not following any communities yet. Browse public threads while Earde is early, or start a pilot community.</p>\
         <div class='launch-empty-actions'>\
         <a class='btn btn--secondary btn--sm' href='/feed?scope=all&sort=%s'>All public threads</a>\
         <a class='btn btn--quiet btn--sm' href='%s'>Start a pilot community</a>\
         </div></div>"
  [ (Html.text sort_mode)
  ; (Html.text mail_pilot) ])
    else
      (Html.static "<div class='empty'>\
       <div class='empty__title'>No public threads yet.</div>\
       <p class='empty__body'>Once communities post, durable discussions surface here.</p>\
       </div>")
  in

  let has_next = List.length posts = 20 in
  let prev_btn = if current_page <= 1 then Html.empty else (Html.template "<a class='btn btn--secondary btn--sm' href='/feed?scope=%s&sort=%s&page=%s'>&larr; Previous</a>"
  [ Html.text scope
  ; Html.text sort_mode
  ; Html.int ((current_page - 1)) ]) in
  let next_btn = if not has_next then Html.empty else (Html.template "<a class='btn btn--secondary btn--sm' href='/feed?scope=%s&sort=%s&page=%s'>Next &rarr;</a>"
  [ Html.text scope
  ; Html.text sort_mode
  ; Html.int ((current_page + 1)) ]) in
  let pager = (Html.template "<div class='pager launch-pager'>%s<span>Page %s</span>%s</div>"
  [ prev_btn
  ; Html.int (current_page)
  ; next_btn ]) in

  let content =
    (Html.template "%s<div class='scroll'><div class='container'>%s\n%s</div></div>"
  [ page_head
  ; posts_html
  ; pager ])
  in

  (* Right aside — factual static product copy + real page data only: ONE
     project-acquisition block (heading, one-sentence body, one CTA — never a
     second Bring/pilot variant), the existing founder contact, and the real
     Following list. No Atlas, no fake trends, no fabricated counts. *)
  let bring_block =
    (Html.static "<div class='aside__block'>\
     <div class='kicker aside__kicker'>Bring your project</div>\
     <p class='aside__text'>Create a dedicated community home for your open-source project, or connect it to an existing community.</p>\
     <a class='btn btn--accent btn--block btn--sm' href='/bring'>Connect a project</a>\
     </div>")
  in
  let founder_block = (Html.template "<div class='aside__block'>\
     <div class='kicker aside__kicker'>Earde is early</div>\
     <p class='aside__text'>If you are trying to build a community here, I want to hear what works, what breaks, and what features you need next.</p>\
     <a class='btn btn--secondary btn--block btn--sm' href='%s' target='_blank' rel='noopener noreferrer'>Talk to me!</a>\
     <div class='launch-aside-alt'><a href='mailto:metacirculardispatches@gmail.com'>or email me</a></div>\
     </div>"
  [ (Html.text founder_tg) ])
  in
  (* Following mini-list — honest, no counts; shown only when the user actually
     follows things. Letter tiles use the shared deterministic launch palette
     (nothing is stored per community). *)
  let tile_glyph slug =
    String.capitalize_ascii
      (if String.length slug >= 2 then String.sub slug 0 2
       else if slug = "" then "?" else slug)
  in
  let following_block =
    if rail_communities = [] then Html.empty
    else
      let rows = Html.concat (List.map (fun (c : Community_types.community) ->
        (Html.template "<a class='navitem' href='/c/%s'><span class='avatar avatar--20' style='background:%s'>%s</span><span class='mono'>/c/%s</span></a>"
  [ (Html.text (c.slug))
  ; (Html.text (Page_shell.launch_tile_color c.slug))
  ; (Html.text ((tile_glyph c.slug)))
  ; (Html.text (c.slug)) ])
      ) rail_communities) in
      (Html.template "<div class='aside__block'><div class='kicker aside__kicker'>Following</div>%s</div>"
  [ rows ])
  in
  let aside = bring_block ++ founder_block ++ following_block in

  Page_shell.launch_app_page ?user ~request ~rail_communities ~aside
    ~page_class:"launch-feed" ~title:"Feed" ~content ()

(* === SEARCH === *)

(* Launch search surface (Cartographic Civic): the same result body the cool-grey
   shell rendered — header form, tabs, sr-* rows, pager, analytics container, all pinned by
   the analytics suites and the replay-masking selectors — re-parented under the shared
   launch app chrome (Components.launch_app_page, body.launch-search).
   Answers "where was this discussed / which thread / which
   community / did it come from chat?" An empty query renders a local prompt state instead
   of redirecting; the route and q/t/page semantics are unchanged. The visible Threads tab
   keeps the internal tab value "posts". chat_sources is the bounded per-page provenance
   lookup (post_id -> channel slug/name/source count) from Thread_source_store.get_thread_sources_for_posts —
   no N+1. rail_communities feeds the launch rail only (viewer membership, same order as
   every other launch surface); it never enters result content. *)
(* user_votes is part of the stable positional API (vote state for the old card renderer); the
   compact search rows show score but no vote arrows, so it is intentionally unused here. *)
let search_results_page ?user ~admin_usernames ?(chat_sources=[]) ?(rail_communities=[]) _user_votes current_page active_tab query (communities: Community_types.community list) users (posts: Post_types.post list) comments request =
  (* Two escaping contexts, applied separately: [eq]/[et] are HTML-escaped for
     text and form-value positions. URLs built for href attributes first
     percent-encode the value as a query parameter (so '&', '=', '#', spaces
     and non-ASCII survive URL parsing intact), and the finished URL is then
     HTML-escaped because it lands inside an attribute. *)
  let q = String.trim query in
  let has_query = q <> "" in
  let eq = (Html.text (q)) in
  let url_q = (Html.text ((Uri.pct_encode ~component:`Query_value q))) in
  let csrf_token = Csrf_field.tag request in
  let et = (Html.text (active_tab)) in
  let url_t = (Html.text ((Uri.pct_encode ~component:`Query_value active_tab))) in

  let chat_source_for id =
    List.find_opt (fun (pid, _, _, _) -> pid = id) chat_sources
  in

  (* --- per-tab row renderers (cool-grey .search-shell idiom) --- *)
  let render_community (a: Community_types.community) =
    (* Reuse the people-result avatar classes (.sr-avatar / --mono) so community and user rows
       align identically; community_avatar falls back to a name letter-tile, never a broken image. *)
    let avatar_html =
      Components.community_avatar ~img_class:"sr-avatar"
        ~tile_class:"sr-avatar sr-avatar--mono" ~name:a.name a.avatar_url
    in
    (Html.template "<a class='sr-row sr-row--link' href='/c/%s'>
        %s
        <div class='sr-row-main'>
          <h3 class='sr-row-title'>%s</h3>
          <div class='sr-row-meta'><span class='sr-c'>/c/%s</span></div>
          <p class='sr-row-excerpt'>%s</p>
        </div>
      </a>"
  [ (Html.text (a.slug))
  ; avatar_html
  ; (Html.text (a.name))
  ; (Html.text (a.slug))
  ; (Html.text ((Option.value ~default:"No description" a.description))) ])
  in

  let render_user (_, username, _, bio, avatar) =
    let eu = (Html.text (username)) in
    (* Same Html.image_src gate + letter-tile fallback as the community row above; replaces the
       prior hand-rolled <img>/tile markup (raw url, only escaped) so a stored unsafe
       avatar_url can't render a broken/hostile src. *)
    let avatar_html =
      Components.user_avatar ~img_class:"sr-avatar"
        ~tile_class:"sr-avatar sr-avatar--mono" ~username avatar
    in
    (Html.template "<a class='sr-row sr-row--link sr-row--user' href='/u/%s'>
        %s
        <div class='sr-row-main'>
          <h3 class='sr-row-title'>u/%s</h3>
          <p class='sr-row-excerpt'>%s</p>
        </div>
      </a>"
  [ eu
  ; avatar_html
  ; eu
  ; (Html.text ((Option.value ~default:"" bio))) ])
  in

  let render_search_comment (_, content, username, created_at, post_id, score) =
    (Html.template "<article class='sr-row'>
        <div class='sr-row-main'>
          <div class='sr-row-meta'>by %s <span class='sr-dot'>·</span> %s <span class='sr-dot'>·</span> <span class='sr-score'>%s</span></div>
          <p class='sr-row-excerpt'>%s</p>
          <a class='sr-row-link' href='/p/%s'>Go to thread &rarr;</a>
        </div>
      </article>"
  [ (Components.render_author ~admin_usernames username)
  ; (Html.text (Components.time_ago created_at))
  ; Html.int (score)
  ; (Html.text (content))
  ; Html.int (post_id) ])
  in

  let render_thread (post: Post_types.post) =
    let section_html = match post.section_name, post.section_slug with
      | Some sn, Some ss ->
          (Html.template " <span class='sr-sep'>&rsaquo;</span> <a class='sr-s' href='/c/%s/s/%s'>%s</a>"
  [ (Html.text (post.community_slug))
  ; (Html.text (ss))
  ; (Html.text (sn)) ])
      | _ -> Html.empty
    in
    let source_html = match chat_source_for post.id with
      | None -> Html.empty
      | Some (_, cslug, cname, n) ->
          let count =
            if n > 0 then (Html.template " <span class='sr-dot'>·</span> from %s chat message%s"
  [ Html.int (n)
  ; (if n = 1 then Html.empty else (Html.static "s")) ])
            else Html.empty
          in
          (Html.template "<div class='sr-row-src'>Started from <a href='/c/%s/ch/%s'>#%s</a>%s</div>"
  [ (Html.text (post.community_slug))
  ; (Html.text (cslug))
  ; (Html.text (cname))
  ; count ])
    in
    (* Same admin/mod forms as the feed, kept in a compact secondary strip so they never
       dominate the public results. post_admin_actions returns "" for non-privileged viewers. *)
    let admin = Post_cards.post_admin_actions ~admin_usernames ~csrf_token request post in
    let admin_html = if admin = Html.empty then Html.empty else (Html.template "<div class='sr-row-admin'>%s</div>"
  [ admin ]) in
    (Html.template "<article class='sr-row'>
        <div class='sr-row-main'>
          <div class='sr-row-meta'><a class='sr-c' href='/c/%s'>/c/%s</a>%s <span class='sr-dot'>·</span> by %s <span class='sr-dot'>·</span> %s</div>
          <h3 class='sr-row-title'><a href='/p/%s'>%s</a></h3>
          %s
          <div class='sr-row-stats'><span class='sr-score'>%s</span> <span class='sr-dot'>·</span> <a href='/p/%s'>%s comment%s</a></div>
        </div>
        %s
      </article>"
  [ (Html.text (post.community_slug))
  ; (Html.text (post.community_slug))
  ; section_html
  ; (Components.render_author ~admin_usernames post.username)
  ; (Html.text (Components.time_ago post.created_at))
  ; Html.int (post.id)
  ; (Html.text (post.title))
  ; source_html
  ; Html.int (post.score)
  ; Html.int (post.id)
  ; Html.int (post.comment_count)
  ; (if post.comment_count = 1 then Html.empty else (Html.static "s"))
  ; admin_html ])
  in

  let empty_state msg =
    (Html.template "<div class='sr-empty'>%s</div>"
  [ msg ])
  in

  let content_html, has_next =
    match active_tab with
    | "communities" ->
        if communities = [] then (empty_state (Html.text (Printf.sprintf "No communities match \"%s\"." (q))), false)
        else ((Html.join (Html.static "\n")) (List.map render_community communities), List.length communities = 20)
    | "people" ->
        if users = [] then (empty_state (Html.text (Printf.sprintf "No people match \"%s\"." (q))), false)
        else ((Html.join (Html.static "\n")) (List.map render_user users), List.length users = 20)
    | "comments" ->
        if comments = [] then (empty_state (Html.text (Printf.sprintf "No comments match \"%s\"." (q))), false)
        else ((Html.join (Html.static "\n")) (List.map render_search_comment comments), List.length comments = 20)
    | _ ->
        if posts = [] then (empty_state (Html.text (Printf.sprintf "No threads match \"%s\"." (q))), false)
        else ((Html.join (Html.static "\n")) (List.map render_thread posts), List.length posts = 20)
  in

  let tab label tab_value =
    let cls = if tab_value = active_tab then "sr-tab is-active" else "sr-tab" in
    (Html.template "<a class='%s' href='/search?q=%s&t=%s'>%s</a>"
  [ (Html.text cls)
  ; url_q
  ; (Html.text tab_value)
  ; label ])
  in
  (* Visible "Threads" maps to the internal tab value "posts" (unchanged route semantics). *)
  let tabs_html = (Html.template "<nav class='sr-tabs'>%s%s%s%s</nav>"
  [ (tab (Html.static "Threads") "posts")
  ; (tab (Html.static "Communities") "communities")
  ; (tab (Html.static "Comments") "comments")
  ; (tab (Html.static "People") "people") ])
  in

  (* PostHog search_performed metadata (spec §2.4): one cohesive, inert
     container of closed values only — never the query text. The tab is
     normalized to the renderer's own closed set (any unknown ?t= value falls
     into the Threads branch above, so it is reported as "posts", never echoed
     back). result_count is the number of rows rendered on THIS page for the
     active tab — the only count the page authoritatively knows: search
     queries are LIMIT/OFFSET and no total-match count exists anywhere. page
     is the effective page after the handler's max-1 clamp. Emitted only when
     analytics is enabled and a non-empty search actually executed. *)
  let analytics_meta_html =
    match Analytics.browser_config () with
    | None -> Html.empty
    | Some _ ->
        let analytics_tab, result_count =
          match active_tab with
          | "communities" -> ("communities", List.length communities)
          | "people" -> ("people", List.length users)
          | "comments" -> ("comments", List.length comments)
          | _ -> ("posts", List.length posts)
        in
        (Html.template "<div id='sr-analytics' hidden data-analytics-search-tab='%s' data-analytics-search-result-count='%s' data-analytics-search-page='%s'></div>"
  [ (Html.text analytics_tab)
  ; Html.int (result_count)
  ; Html.int ((max 1 current_page)) ])
  in

  let prev_btn = if current_page <= 1 then Html.empty else
    (Html.template "<a class='sr-page' href='/search?q=%s&t=%s&page=%s'>&larr; Prev</a>"
  [ url_q
  ; url_t
  ; Html.int ((current_page - 1)) ]) in
  let next_btn = if not has_next then Html.empty else
    (Html.template "<a class='sr-page' href='/search?q=%s&t=%s&page=%s'>Next &rarr;</a>"
  [ url_q
  ; url_t
  ; Html.int ((current_page + 1)) ]) in

  (* The search header (label + input) is always present; tabs/results only when a query exists. *)
  let header_html = (Html.template "
    <form class='sr-head' action='/search' method='GET'>
      <label class='sr-label' for='sr-q'>Search</label>
      <div class='sr-inputrow'>
        <span class='sr-sigil'>/</span>
        <input id='sr-q' class='sr-input' type='text' name='q' value='%s' placeholder='grep public threads · communities · people…' autocomplete='off' autofocus>
        <input type='hidden' name='t' value='%s'>
        <button class='sr-go' type='submit'>search</button>
      </div>
    </form>"
  [ eq
  ; et ])
  in

  let body =
    if not has_query then
      (Html.template "<div class='sr-wrap'>%s<div class='sr-empty sr-empty--prompt'>Search Earde's public archive — threads, communities, comments and people.</div></div>"
  [ header_html ])
    else
      (Html.template "<div class='sr-wrap'>
          %s
          %s
          <div class='sr-results'>%s</div>
          <div class='sr-pager'>%s%s</div>
          %s
        </div>"
  [ header_html
  ; tabs_html
  ; content_html
  ; prev_btn
  ; next_btn
  ; analytics_meta_html ])
  in
  (* Generic on purpose: the document <title> leaks into analytics surfaces
     (replay snapshots, $title) — the search term must never appear there.
     The visible UI still echoes the query via the input value (masked). *)
  let title = "Search" in
  (* Standard launch scroller column around the untouched .sr-wrap fragment;
     the serif page heading is the existing .sr-label, restyled in the
     launch-search CSS section rather than duplicated here. *)
  let content =
    (Html.template "<div class='scroll'><div class='container container--list'>%s</div></div>"
  [ body ])
  in
  Page_shell.launch_app_page ?user ~request ~rail_communities
    ~page_class:"launch-search" ~title ~content ()
