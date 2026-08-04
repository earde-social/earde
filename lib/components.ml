(* Bound before `open Db`, which would otherwise shadow the top-level
   Analytics module with Db.Analytics. *)
module Posthog = Analytics

open Db

(* Defense-in-depth: escape before string-interpolation into HTML templates.
   Caqti prevents SQLi; this prevents stored/reflected XSS. *)
let html_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (function
    | '&'  -> Buffer.add_string buf "&amp;"
    | '<'  -> Buffer.add_string buf "&lt;"
    | '>'  -> Buffer.add_string buf "&gt;"
    | '"'  -> Buffer.add_string buf "&quot;"
    | '\'' -> Buffer.add_string buf "&#39;"
    | c    -> Buffer.add_char buf c) s;
  Buffer.contents buf

(* Block javascript: and data: URLs — only http(s) are safe as user-supplied link targets.
   Falls back to "#" so the anchor renders but is inert. *)
let safe_url url =
  let lower = String.lowercase_ascii url in
  if (String.length lower >= 7 && String.sub lower 0 7 = "http://")
  || (String.length lower >= 8 && String.sub lower 0 8 = "https://")
  then html_escape url else "#"

(* Internal nav targets (sidebar channel/section links, rail tiles) are server-built app paths
   like "/c/europe/ch/general" — NOT user input. They must NOT go through safe_url: that only
   passes http(s):// and would collapse every relative path to "#" (the bug that made the
   community sidebar links inert). This is the dedicated check for rooted internal paths:
   require a single leading "/", reject "" / "#", and reject protocol-relative "//host" (and the
   "/\\host" backslash variant some engines normalise to it) so it can never become an open
   redirect. javascript:/data: can't start with "/" so they're rejected implicitly. *)
let safe_internal_path path =
  if String.length path >= 2
     && path.[0] = '/'
     && path.[1] <> '/' && path.[1] <> '\\'
  then html_escape path else "#"

(* === IMAGES ===
   One escaping gate + a few render primitives so every image surface treats stored URLs the
   same way. Before this, image src was rendered three different ways (raw, html_escape, and
   safe_url), and safe_url — correct for href — silently broke local uploads (it only passes
   http(s):// and collapses "/static/uploads/x.webp" to "#"). *)

(* The dedicated gate for image src attributes. Unlike safe_url (link href, http(s) only) it
   ALSO passes rooted local app paths so uploaded images render — the upload pipeline stores
   "/static/uploads/<name>.webp". Rejects (→ "#"): javascript:/data:, protocol-relative
   "//host" and the "/\\host" variant, empty/whitespace, AND any candidate containing a quote,
   angle bracket, backtick, ASCII whitespace, or control char. Rather than leaning on
   html_escape to neutralise an injection payload after the fact, we refuse to emit it at all —
   a legitimate upload path / URL never contains these chars, so this only ever rejects hostile
   or malformed input. (html_escape stays as belt-and-suspenders on the accepted value.) *)
let safe_img_src raw =
  let url = String.trim raw in
  let has_dangerous_char =
    String.exists (fun c ->
      match c with
      | '\'' | '"' | '<' | '>' | '`' -> true
      | ' ' | '\t' | '\n' | '\r' -> true
      | c when Char.code c < 0x20 || Char.code c = 0x7f -> true
      | _ -> false) url
  in
  if url = "" || has_dangerous_char then "#"
  else
    let lower = String.lowercase_ascii url in
    let is_http =
      (String.length lower >= 7 && String.sub lower 0 7 = "http://")
      || (String.length lower >= 8 && String.sub lower 0 8 = "https://")
    in
    (* Rooted local path: single leading "/", not "//host" and not "/\\host". javascript:/data:
       can't start with "/", so they're rejected implicitly here. *)
    let is_local =
      String.length url >= 2 && url.[0] = '/' && url.[1] <> '/' && url.[1] <> '\\'
    in
    if is_http || is_local then html_escape url else "#"

(* First-letter glyph for a name/username; "?" when empty. html_escape'd so a one-char name
   like "<" can't inject. String.sub 0 1 matches the existing letter-tile code (byte-based;
   a leading multibyte char degrades to "?"-ish like before, not a crash). *)
let initial_glyph name =
  let n = String.trim name in
  if n = "" then "?"
  else html_escape (String.uppercase_ascii (String.sub n 0 1))

(* The single letter-tile fallback. [class_] carries the surface's existing utility classes
   (size/shape/color/centering) so each call site keeps its exact look — this only centralizes
   the first-letter extraction + escaping that was hand-rolled per surface. *)
let initial_tile ?(class_="") name =
  let cls = if class_ = "" then "" else " class='" ^ class_ ^ "'" in
  Printf.sprintf "<div%s>%s</div>" cls (initial_glyph name)

(* <img> when the avatar URL is safe and non-empty, else the letter tile. A stored value that
   fails safe_img_src (e.g. an injected javascript: payload) falls back to the tile rather than
   rendering a dead src='#'. [img_class] styles the <img>, [tile_class] the fallback <div>. *)
let user_avatar ?(alt="") ~img_class ~tile_class ~username avatar_url =
  match avatar_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then initial_tile ~class_:tile_class username
      else Printf.sprintf "<img src='%s' class='%s' alt='%s'>" src img_class (html_escape alt)
  | _ -> initial_tile ~class_:tile_class username

(* Community counterpart of user_avatar; the tile glyph is the community name's first letter. *)
let community_avatar ?(alt="") ~img_class ~tile_class ~name avatar_url =
  match avatar_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then initial_tile ~class_:tile_class name
      else Printf.sprintf "<img src='%s' class='%s' alt='%s'>" src img_class (html_escape alt)
  | _ -> initial_tile ~class_:tile_class name

(* Banner image when present & safe, else the caller's existing fallback element (e.g. a
   gradient block). [wrap_class] wraps the <img> case; [fallback_class] is the empty
   placeholder div — both preserve the current home-hero markup when passed its class strings. *)
let community_banner ~wrap_class ~img_class ~fallback_class banner_url =
  match banner_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then Printf.sprintf "<div class='%s'></div>" fallback_class
      else Printf.sprintf "<div class='%s'><img src='%s' class='%s' alt='banner'></div>"
             wrap_class src img_class
  | _ -> Printf.sprintf "<div class='%s'></div>" fallback_class

(* Small post image for rows; "" when there is no image (so callers can concatenate freely).
   Added for the later feed/search slice — not wired broadly in this slice. *)
let post_thumbnail ?(alt="Post image") ~img_class image_url =
  match image_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then ""
      else Printf.sprintf "<img src='%s' class='%s' alt='%s'>" src img_class (html_escape alt)
  | _ -> ""

(* === LAYOUT ===
   Every live document is a complete, self-contained Cartographic Civic
   launch page (the launch_* wrappers below). They share exactly two
   ingredients — the desktop-only mobile gate and the analytics assets —
   and load no external stylesheet, font or script. *)

(* Desktop-only gate. MVP is desktop-first: rather than making the multi-pane app shell
   responsive, a phone-width viewport gets a centered "open on desktop" panel instead of the
   app. Pure CSS + SSR — no JS, no user-agent sniffing, no separate route. mobile-gate.css hides
   the app chrome and reveals this panel under the breakpoint; on wide screens the panel is
   display:none and nothing changes. Injected only on `App surfaces (see layout); auth/legal/
   marketing pages are left usable. The HTML always renders, so crawlers/SSR are unaffected. *)
let mobile_gate_css_link = "<link rel='stylesheet' href='/static/css/mobile-gate.css'>"
let mobile_desktop_gate =
  "<div class='mobile-gate' role='dialog' aria-label='Desktop only'>\
     <div class='mobile-gate-card'>\
       <div class='mobile-gate-brand'><img src='/static/images/logo-mark.svg' alt='' class='mobile-gate-logo-mark'><img src='/static/images/logo-wordmark.svg' alt='Earde' class='mobile-gate-logo-wordmark'></div>\
       <h1 class='mobile-gate-title'>Desktop only for now</h1>\
       <p class='mobile-gate-body'>Earde is currently built for laptop and desktop screens. Open this page on a larger screen to use the full app.</p>\
       <p class='mobile-gate-note'>Mobile support is coming later.</p>\
     </div>\
   </div>"

(* PostHog browser integration (spec §2): emitted only when analytics is
   enabled with valid public config — otherwise no script, no banner, no
   attributes, and therefore no PostHog network request. Only the public
   token and ingest host are rendered; analytics.js itself is local and
   connects to PostHog exclusively after granted consent. The banner ships
   hidden; analytics.js reveals it only when no consent cookie exists.
   The same root carries the §4.2 identity attribute (authenticated pages
   only: exactly user:<id>, nothing else — no username/email/raw id) and
   the §5.3 group attribute (community-scoped pages only: exactly
   community:<id>, keyed by the immutable numeric id). Shared by every
   launch_* document so all of them carry the exact same analytics assets. *)
let analytics_assets ?request ?analytics_community () =
  match Posthog.browser_config () with
    | None -> ("", "")
    | Some cfg ->
        let identity_attr =
          match request with
          | None -> ""
          | Some req -> (
              (* Absence of session middleware (tests) or a malformed session
                 value must safely mean "no identity". *)
              match (try Dream.session_field req "user_id" with _ -> None) with
              | None -> ""
              | Some uid_str -> (
                  match int_of_string_opt uid_str with
                  | Some id ->
                      Printf.sprintf " data-analytics-user='%s'"
                        (html_escape (Posthog.distinct_id_of_user_id id))
                  | None -> ""))
        in
        (* Community context arrives as (id, authoritative visibility) — the
           pair comes from the Db.community record the page already holds, so
           the §13 private marker can never be derived from URL shape and a
           call site cannot pass the id without stating visibility. The
           marker value is only "true": no name, no slug. On marked
           documents analytics.js records consent but never loads the SDK. *)
        let group_attr, private_attr =
          match analytics_community with
          | Some (community_id, visibility) ->
              ( Printf.sprintf " data-analytics-group='%s'"
                  (html_escape (Posthog.community_group_key community_id)),
                if Db.community_is_private visibility then
                  " data-analytics-private-community='true'"
                else "" )
          | None -> ("", "")
        in
        ( "<script src='/static/js/analytics.js' defer></script>",
          Printf.sprintf
            "<div id='analytics-consent' hidden data-ph-token='%s' data-ph-api-host='%s' data-ph-deployment-environment='%s'%s%s%s class='fixed bottom-4 left-1/2 -translate-x-1/2 z-50 w-[calc(100%%-2rem)] max-w-md bg-white border border-[#E0D9CC] rounded-2xl shadow-xl p-4'>\
               <p class='text-sm text-gray-700 mb-3'>Earde can collect optional usage analytics (PostHog) to improve the product. Nothing is collected until you choose, and you can change your choice at any time on the <a href='/privacy#cookies-analytics'>privacy page</a>.</p>\
               <div class='flex items-center gap-2'>\
                 <button type='button' data-analytics-accept class='px-4 py-1.5 text-sm font-semibold bg-[#C94C4C] text-white rounded-full hover:bg-[#A83A3A] transition'>Accept</button>\
                 <button type='button' data-analytics-refuse class='px-4 py-1.5 text-sm font-semibold text-gray-600 bg-gray-100 rounded-full hover:bg-gray-200 transition'>Refuse</button>\
                 <span data-analytics-error hidden class='text-xs text-red-600'>Couldn&#39;t save &mdash; try again.</span>\
               </div>\
             </div>"
            (html_escape cfg.Posthog.browser_token)
            (html_escape cfg.Posthog.browser_api_host)
            (html_escape cfg.Posthog.browser_deployment_environment)
            identity_attr group_attr private_attr )

(* The pre-launch page furniture (the warm Tailwind `Site navbar + footer, the
   mono `App command bar, and the focused `Auth card) is gone. Every live
   document is now one of the self-contained Cartographic Civic launch
   wrappers below: they load /static/css/earde.css (plus the shared
   desktop-only mobile gate on app surfaces) and nothing external. *)

(* The persistent top-right launch-topbar action: the compact, always-present
   entry point to /bring. One shared value — rendered identically to
   anonymous and authenticated viewers — so the launch documents below
   cannot drift apart.

   The mark is the same local GitHub path the /bring start button draws, at
   topbar scale (15px) and inheriting the button's white foreground through
   fill='currentColor' — no external asset, no icon font, no emoji. The mark
   is decorative (aria-hidden), so the accessible name is exactly the visible
   label "Connect a project" — no aria-label that could drift from the text.
   The element stays a plain same-tab <a href='/bring'> — the /bring page
   keeps the explicit full-label primary action, and suppresses this compact
   one because it would self-link. *)
let launch_connect_cta =
  "<a class='btn btn--connect-github' href='/bring' \
   title='Connect an open-source project'>\
   <svg width='15' height='15' viewBox='0 0 16 16' fill='currentColor' \
   aria-hidden='true'><path d='M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 \
   5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 \
   1.08.58 1.23.82.72 1.21 1.87.87 \
   2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 \
   0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.6 \
   7.6 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 \
   2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 \
   3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 \
   .21.15.46.55.38A8.01 8.01 0 0 0 16 8c0-4.42-3.58-8-8-8Z'/></svg>\
   <span>Connect a project</span></a>"

(* The one global footer: a slim, viewer-independent legal strip rendered as
   the .app column's last child (below .shell) by every chrome-bearing launch
   wrapper — entry, auth, app, onboarding and both community documents. It
   deliberately sits OUTSIDE <main>: the onboarding/settings feature suites
   slice fragments as create-shell/cm-main → </main>, and the chat surface's
   .cs-main flex column must keep its exact direct children, so nothing may
   be appended inside a main element. The message sheet keeps its documented
   no-chrome contract and renders no footer. Static internal links only —
   byte-identical for anonymous and authenticated viewers, no forms, no
   scripts, and the Analytics-preferences target is the /privacy section
   hosting the working consent controls. *)
let launch_footer =
  "<footer class='launch-footer'>\
   <a href='/privacy'>Privacy</a>\
   <span class='launch-footer__sep' aria-hidden='true'>&middot;</span>\
   <a href='/privacy#cookies-analytics'>Analytics preferences</a>\
   </footer>"

(* The one anonymous topbar action cluster: the shared Connect CTA plus the
   two authentication controls. One value used by every launch wrapper with
   an anonymous arm, so the anonymous and authenticated topbars render the
   byte-identical CTA and only the surrounding controls differ. *)
let topbar_anon_actions =
  launch_connect_cta
  ^ "<a class='btn btn--secondary btn--auth' href='/login'>Log in</a>\
     <a class='btn btn--primary btn--auth' href='/signup'>Sign up</a>"

(* Entry-chrome topbar policy, chosen per route:
   - [Entry_connect_cta] (default): the viewer-independent chrome — the shared
     Connect CTA alone, no auth controls (/privacy).
   - [Entry_viewer user]: viewer-dependent auth controls and NO Connect CTA —
     /bring only, where the compact CTA would self-link. Anonymous viewers get
     the two auth links; members get the bell and the plain user-chip link
     (never the <details> menu with its logout POST: /bring's non-ready
     states assert zero <form> elements document-wide). *)
type entry_topbar =
  | Entry_connect_cta
  | Entry_viewer of string option

(* Cartographic Civic launch entry document (pass 1: /bring only). A complete,
   self-contained HTML document that loads only the launch stylesheet
   (earde.css) — no Tailwind, no external fonts, no legacy per-page CSS.

   The chrome is form-free: /bring's non-ready states assert zero <form>
   elements across the whole document, and its authenticated Off state
   additionally forbids a /login link, so the top bar renders the command
   field as a link to /search and offers no search form and no logout.
   Every link is a real route (/feed, /search, /bring, /login, /signup,
   /notifications, /u/:name — the last four only under [Entry_viewer] — plus
   the shared [launch_footer]'s /privacy links).

   Analytics behavior is the shared [analytics_assets] (script + consent
   banner); the scoped integration CSS at the end of earde.css positions the
   banner, whose utility classes are inert without Tailwind. [page_class] is the route-specific scoping root for
   that integration CSS (e.g. "launch-bring"), stamped on <body>. *)
let launch_entry_page ?(noindex = false) ?request ?(topbar = Entry_connect_cta)
    ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  let house_icon =
    "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>"
  in
  let bell_icon =
    "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>"
  in
  let actions =
    match topbar with
    | Entry_connect_cta -> launch_connect_cta
    | Entry_viewer (Some username) ->
        let u = html_escape username in
        let initial =
          if String.length username > 0
          then html_escape (String.sub (String.uppercase_ascii username) 0 1)
          else "?"
        in
        (* Form-free member cluster (the onboarding wrapper's, minus the CTA):
           badge server-rendered from the request's unread count, user chip a
           plain link — no <details> menu, no logout form. *)
        Printf.sprintf
          "<a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <a class='userchip' href='/u/%s'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></a>"
          bell_icon
          (Notification_badge.badge_html ?request ())
          u initial u
    | Entry_viewer None ->
        "<a class='btn btn--secondary btn--auth' href='/login'>Log in</a>\
         <a class='btn btn--primary btn--auth' href='/signup'>Sign up</a>"
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     </head>\n\
     <body class='%s'>\n\
     <div class='app'>\n\
     <header class='topbar'>\
     <a class='topbar__brand' href='/feed' aria-label='Earde feed'>\
     <img class='topbar__mark' src='/static/images/logo-mark.svg' alt=''>\
     <img class='topbar__wordmark' src='/static/images/logo-wordmark.svg' alt='Earde'>\
     </a>\
     <a class='search launch-search' href='/search' aria-label='Search Earde'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <span class='launch-search__hint'>grep threads &middot; projects &middot; communities&hellip;</span>\
     <span class='launch-search__enter' aria-hidden='true'>&#8629;</span>\
     </a>\
     <div class='topbar__actions'>%s</div>\
     </header>\n\
     <div class='shell'>\
     <nav class='rail' aria-label='Primary'>\
     <a class='rail__item' href='/feed' title='Feed' aria-label='Feed'>%s</a>\
     <span class='rail__spacer'></span>\
     <a class='rail__item rail__item--add' href='/bring' title='Connect a project' aria-label='Connect a project'>&#65291;</a>\
     </nav>\
     <main class='main'><div class='scroll'><div class='container--form'>\n\
     %s\n\
     </div></div></main>\
     </div>\n\
     %s\n\
     </div>\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta analytics_head page_class
    actions house_icon content launch_footer analytics_banner

(* Cartographic Civic launch auth document (pass 2: /login and /signup only).
   Like [launch_entry_page], a complete self-contained document that loads only
   earde.css — no Tailwind, no external fonts, no legacy per-page CSS, no
   mobile gate, no notification polling — but with the approved *anonymous* top bar (the shared
   Connect-a-project CTA / Log in / Sign up) instead of the entry chrome, and no icon rail:
   the /login and /signup routes render no panes at all (04-ROUTES).

   The chrome is deterministic and viewer-independent: the same three real
   routes for every request, no bell, no user chip, no logout form. The
   command field stays a styled link to /search (same trick as pass 1) so this
   wrapper adds no form contract beyond the page's own auth form. Analytics
   behavior is the shared [analytics_assets], identical to
   [launch_entry_page]. [page_class] ("launch-login" / "launch-signup") lands
   on <body> next to the shared "launch-auth" scope root that the integration
   CSS at the end of earde.css keys on. Existing wrappers and their callers
   are untouched. *)
let launch_auth_page ?(noindex = false) ?request ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     </head>\n\
     <body class='launch-auth %s'>\n\
     <div class='app'>\n\
     <header class='topbar'>\
     <a class='topbar__brand' href='/feed' aria-label='Earde feed'>\
     <img class='topbar__mark' src='/static/images/logo-mark.svg' alt=''>\
     <img class='topbar__wordmark' src='/static/images/logo-wordmark.svg' alt='Earde'>\
     </a>\
     <a class='search launch-search' href='/search' aria-label='Search Earde'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <span class='launch-search__hint'>grep threads &middot; projects &middot; communities&hellip;</span>\
     <span class='launch-search__enter' aria-hidden='true'>&#8629;</span>\
     </a>\
     <div class='topbar__actions topbar__actions--anon'>%s</div>\
     </header>\n\
     <div class='shell'>\
     <main class='main main--paper'><div class='scroll'>\n\
     %s\n\
     </div></main>\
     </div>\n\
     %s\n\
     </div>\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta analytics_head page_class
    topbar_anon_actions content launch_footer analytics_banner

(* Cartographic Civic launch message document (pass 17: the shared
   Pages.msg_page only). Like the other launch documents, complete and
   self-contained, loading only earde.css — no Tailwind, no external fonts,
   no legacy per-page CSS, no mobile gate, no behavior script, no
   notification wiring —
   but with NO chrome at all beyond the paper shell: no top bar, no rail, no
   sidebar, no footer, no forms. ~400 handler call sites across every status
   family (200/400/403/404/409/429/500) share this one document, several of
   them under byte-identity anti-enumeration pins (existing-private vs
   missing resources), so the wrapper must stay strictly viewer- and
   resource-independent: the only per-render variation is [title] and
   [content], both caller-supplied. The single body class is the neutral
   "launch-message-page" — never status- or resource-derived. Analytics
   behavior is the shared [analytics_assets], identical to the other launch
   wrappers; msg_page has always rendered without a robots
   meta, so [noindex] keeps the same default. *)
let launch_message_page ?(noindex = false) ?request ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     </head>\n\
     <body class='launch-message-page'>\n\
     <div class='app'>\n\
     <div class='shell'>\
     <main class='main main--paper'><div class='scroll'>\n\
     %s\n\
     </div></main>\
     </div>\n\
     </div>\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta analytics_head content analytics_banner

(* Deterministic launch-palette colour for a community tile/avatar. The
   database stores no per-community colour, so the launch chrome derives a
   stable presentational value from the slug alone (same slug → same colour
   on every render, no randomness, nothing persisted). The palette is the
   five approved Cartographic Civic community tones from the handoff
   reference (moss / clay / ochre / slate / forest families). *)
let launch_tile_color slug =
  let palette = [| "#556B57"; "#A15E3B"; "#B39352"; "#4D6A73"; "#3E5143" |] in
  let sum = ref 0 in
  String.iter (fun c -> sum := !sum + Char.code c) slug;
  palette.(!sum mod Array.length palette)

(* The behavior script members' launch app documents carry: the confirm modal
   for the own-post delete form, share-link copy, and optimistic voting over
   form[action='/vote'] / form[action='/vote-comment']. It is the pre-launch
   inline script carried over, so every DOM contract still holds
   (parentElement traversal, first/lastElementChild vote forms, the colour
   class names the vote handler toggles). Anonymous launch documents omit it
   entirely: they render no vote forms and no ⋯ menu, so there is nothing for
   it to do.

   It no longer fetches the notification count. That badge is now rendered by
   the server from the request's own unread count ([Notification_badge]), so
   there is one answer per page load instead of a hard-coded zero that a
   later fetch may or may not correct. *)
(* The one copyPostLink definition, shared verbatim by the authenticated
   behavior script below and the guest-only [launch_share_script] — the
   Share button render_post emits is the SAME byte-pinned control for both
   viewer states, so the two documents must never ship diverging copies. *)
let launch_share_snippet = {js|        /* Clipboard write is async; we optimistically swap innerHTML and class list
           rather than disabling the button — avoids layout shift on fast connections. */
        function copyPostLink(path, btn) {
          var fullUrl = window.location.origin + path;
          navigator.clipboard.writeText(fullUrl).then(function() {
            var originalHTML = btn.innerHTML;
            btn.innerHTML = '<svg class="w-4 h-4 mr-1 inline" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M5 13l4 4L19 7"></path></svg> Copied';
            btn.classList.add('text-emerald-600');
            btn.classList.remove('text-gray-500', 'hover:text-gray-900');
            setTimeout(function() {
              btn.innerHTML = originalHTML;
              btn.classList.remove('text-emerald-600');
              btn.classList.add('text-gray-500', 'hover:text-gray-900');
            }, 2000);
          }).catch(function(err) { console.error('Failed to copy: ', err); });
        }|js}

(* Guest-only public-interaction script: exactly the shared copyPostLink and
   nothing else — no confirm modal and no vote handler (anonymous documents
   must fire zero requests beyond assets). Emitted per route (currently the flat community home,
   whose byte-pinned rows show Share to every viewer); the doc builders'
   anonymous default stays script-free. *)
let launch_share_script = "<script>\n" ^ launch_share_snippet ^ "\n      </script>"

let launch_behavior_script = {js|<script>
        /* Custom confirmation modal: replaces native window.confirm() — the browser's
           built-in dialog is synchronous, unstyled, and blocks the JS thread. */
        function confirmModal(event, message) {
          event.preventDefault();
          const form = event.target;
          const overlay = document.createElement('div');
          overlay.className = 'fixed inset-0 bg-gray-900/40 backdrop-blur-sm z-50 flex items-center justify-center opacity-0 transition-opacity duration-200';
          const modal = document.createElement('div');
          modal.className = 'bg-white rounded-2xl shadow-xl p-6 max-w-sm w-full mx-4 transform scale-95 transition-transform duration-200';
          modal.innerHTML = `
            <h3 class='text-lg font-semibold text-gray-900 mb-2'>Are you sure?</h3>
            <p class='text-sm text-gray-500 mb-6' id='modal-confirm-msg'></p>
            <div class='flex justify-end gap-3'>
              <button type='button' class='px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-xl hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#C94C4C]' id='cancel-btn'>Cancel</button>
              <button type='button' class='px-4 py-2 text-sm font-medium text-white bg-red-600 border border-transparent rounded-xl hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-600' id='confirm-btn'>Confirm</button>
            </div>`;
          /* textContent prevents innerHTML XSS — message may contain user-supplied
             usernames (e.g., ban dialog). Using textContent treats the value as
             plain text regardless of what it contains. */
          modal.querySelector('#modal-confirm-msg').textContent = message;
          overlay.appendChild(modal);
          document.body.appendChild(overlay);
          requestAnimationFrame(() => {
            overlay.classList.remove('opacity-0');
            modal.classList.remove('scale-95');
          });
          const close = () => {
            overlay.classList.add('opacity-0');
            modal.classList.add('scale-95');
            setTimeout(() => overlay.remove(), 200);
          };
          document.getElementById('cancel-btn').onclick = close;
          /* form.submit() bypasses the submit event so onsubmit won't re-fire. */
          document.getElementById('confirm-btn').onclick = () => { close(); form.submit(); };
        }
|js} ^ launch_share_snippet ^ {js|
        /* Optimistic vote update: mutate DOM immediately, then fire-and-forget XHR.
           If the request fails the server state is authoritative on next page load —
           acceptable UX trade-off for a forum where stale scores are low-stakes. */
        document.querySelectorAll("form[action='/vote'], form[action='/vote-comment']").forEach(form => {
            form.addEventListener("submit", async (e) => {
                e.preventDefault();
                const formData = new FormData(form);
                const urlEncodedData = new URLSearchParams(formData).toString();

                fetch(form.action, {
                    method: 'POST',
                    headers: {'Content-Type': 'application/x-www-form-urlencoded'},
                    body: urlEncodedData
                });

                const container = form.parentElement;
                const scoreSpan = container.querySelector("span");
                let score = parseInt(scoreSpan.innerText);

                const upForm = container.firstElementChild;
                /* downForm may be absent when allow_downvotes=false — guard against
                   lastElementChild being the score <span> rather than a vote form. */
                const lastEl = container.lastElementChild;
                const downForm = (lastEl && lastEl.tagName === 'FORM') ? lastEl : null;

                const upBtn = upForm?.querySelector("button");
                const downBtn = downForm?.querySelector("button");

                const upInput = upForm?.querySelector("input[name='direction']");
                const downInput = downForm?.querySelector("input[name='direction']");

                const action = parseInt(formData.get("direction"));
                const isUpvoteBtn = form === upForm;

                const resetColors = () => {
                    upBtn?.classList.remove("text-orange-500");
                    upBtn?.classList.add("text-gray-400", "hover:text-orange-500");
                    downBtn?.classList.remove("text-[#69C3D2]");
                    downBtn?.classList.add("text-gray-400", "hover:text-[#69C3D2]");
                };

                if (action === 1) {
                    /* downInput===null means downvotes disabled → no prior downvote possible */
                    if (downInput && parseInt(downInput.value) === 0) score += 2;
                    else score += 1;
                    resetColors();
                    upBtn?.classList.remove("text-gray-400", "hover:text-orange-500");
                    upBtn?.classList.add("text-orange-500");
                    if (upInput) upInput.value = "0";
                    if (downInput) downInput.value = "-1";
                }
                else if (action === -1) {
                    if (upInput && parseInt(upInput.value) === 0) score -= 2;
                    else score -= 1;
                    resetColors();
                    downBtn?.classList.remove("text-gray-400", "hover:text-[#69C3D2]");
                    downBtn?.classList.add("text-[#69C3D2]");
                    if (upInput) upInput.value = "1";
                    if (downInput) downInput.value = "0";
                }
                else if (action === 0) {
                    if (isUpvoteBtn) score -= 1;
                    else score += 1;
                    resetColors();
                    if (upInput) upInput.value = "1";
                    if (downInput) downInput.value = "-1";
                }

                scoreSpan.innerText = score;
            });
        });
      </script>|js}

(* Cartographic Civic launch app document (pass 3: /feed only). Like the pass
   1/2 documents, a complete self-contained page loading only earde.css — no
   Tailwind, no external fonts, no legacy per-page CSS — but with the full app
   chrome: the 54px top bar (brand → /feed, a REAL /search form, viewer-state
   actions), the dark 64px icon rail (Feed active with its bleeding marker,
   one tile per real joined community, ＋ → /bring), the central main column,
   and an optional right aside. Unlike the entry/auth documents it also
   carries the desktop-only mobile gate (the shared stylesheet + panel) and,
   for members only, the shared launch behavior
   script (optimistic voting, confirm modal) —
   see [launch_behavior_script]. Community tiles keep the existing rail
   destination (/c/:slug/ch/general) and face fallback (avatar image when it
   passes the gate, else the 2-letter slug glyph) so the launch rail offers
   exactly the capabilities the pre-launch rail did. Every link is a real
   route. *)
let launch_app_page ?(noindex = false) ?request ?user ?(rail_communities = [])
    ?analytics_community ?(aside = "") ~page_class ~title ~content () =
  let analytics_head, analytics_banner =
    analytics_assets ?request ?analytics_community ()
  in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  let is_admin = match request with
    | Some req -> (try Dream.session_field req "is_admin" = Some "true" with _ -> false)
    | None -> false
  in
  let house_icon =
    "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>"
  in
  let bell_icon =
    "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>"
  in
  (* Same /search route + ?q= contract (and `required`) as both legacy search
     forms; only the skin is the handoff command field. *)
  let search_form =
    "<form class='topbar__search-cell' action='/search' method='GET' role='search'>\
     <div class='search'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <label class='sr-only' for='q'>Search Earde</label>\
     <input class='search__input' id='q' type='text' name='q' required placeholder='grep threads &middot; projects &middot; communities&hellip;'>\
     <button class='search__enter' type='submit' aria-label='Search'>&#8629;</button>\
     </div>\
     </form>"
  in
  let actions =
    match user with
    | Some username ->
        let u = html_escape username in
        (* /admin is admin-only; the handler re-checks is_admin, so offering
           the link to a flagged session leaks nothing. *)
        let admin_item = if is_admin then "<a href='/admin'>Admin</a>" else "" in
        let initial =
          if String.length username > 0
          then html_escape (String.sub (String.uppercase_ascii username) 0 1)
          else "?"
        in
        (* User menu is a pure-CSS <details>; Log out stays a POST form with
           its existing action/semantics. The bell's badge is server-rendered
           from the request's unread count and is simply absent at zero — see
           [Notification_badge]. *)
        Printf.sprintf
          "%s\
           <a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <details class='launch-user'>\
           <summary class='userchip'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></summary>\
           <div class='launch-user__menu'>\
           <a href='/u/%s'>Profile</a>\
           <a href='/settings'>Settings</a>\
           <a href='/notifications'>Notifications</a>\
           %s\
           <form action='/logout' method='POST'><button type='submit'>Log out</button></form>\
           </div>\
           </details>"
          launch_connect_cta bell_icon
          (Notification_badge.badge_html ?request ())
          initial u u admin_item
    | None ->
        (* Anonymous cluster (04-ROUTES): no bell, no user chip, no logout,
           and therefore no notification fetch anywhere in the document. *)
        topbar_anon_actions
  in
  (* Same face fallback as the legacy rail's rail_glyph (defined later in
     this file): first two slug letters, one if short, "?" if empty — only
     capitalized here to match the approved tile face. *)
  let tile_glyph slug =
    let raw =
      if String.length slug >= 2 then String.sub slug 0 2
      else if String.length slug = 1 then slug
      else "?"
    in
    String.capitalize_ascii raw
  in
  let tiles =
    List.map
      (fun (c : community) ->
        let face =
          match c.avatar_url with
          | Some url when String.trim url <> "" ->
              let src = safe_img_src url in
              if src = "#" then html_escape (tile_glyph c.slug)
              else Printf.sprintf "<img class='launch-rail__img' src='%s' alt=''>" src
          | _ -> html_escape (tile_glyph c.slug)
        in
        Printf.sprintf
          "<a class='rail__item rail__item--community' href='/c/%s/ch/general' title='/c/%s' style='background:%s'>%s</a>"
          (html_escape c.slug) (html_escape c.slug) (launch_tile_color c.slug) face)
      rail_communities
  in
  let divider =
    if rail_communities = [] then "" else "<span class='rail__divider'></span>"
  in
  let rail =
    Printf.sprintf
      "<nav class='rail' aria-label='Primary'>\
       <a class='rail__item' href='/feed' title='Feed' aria-label='Feed'><span class='rail__marker'></span>%s</a>\
       %s%s\
       <span class='rail__spacer'></span>\
       <a class='rail__item rail__item--add' href='/bring' title='Connect a project' aria-label='Connect a project'>&#65291;</a>\
       </nav>"
      house_icon divider (String.concat "" tiles)
  in
  let aside_html =
    if aside = "" then ""
    else Printf.sprintf "<aside class='aside' aria-label='Secondary'>%s</aside>" aside
  in
  let behavior_script = match user with
    | Some _ -> launch_behavior_script
    | None -> ""
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     %s\n\
     </head>\n\
     <body class='%s'>\n\
     <div class='app'>\n\
     <header class='topbar'>\
     <a class='topbar__brand' href='/feed' aria-label='Earde feed'>\
     <img class='topbar__mark' src='/static/images/logo-mark.svg' alt=''>\
     <img class='topbar__wordmark' src='/static/images/logo-wordmark.svg' alt='Earde'>\
     </a>\
     %s\
     <div class='topbar__actions'>%s</div>\
     </header>\n\
     <div class='shell'>\
     %s\
     <main class='main'>\n\
     %s\n\
     </main>\
     %s\
     </div>\n\
     %s\n\
     </div>\n\
     %s\n\
     %s\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta mobile_gate_css_link analytics_head
    page_class search_form actions rail content aside_html launch_footer
    mobile_desktop_gate analytics_banner behavior_script

(* Cartographic Civic launch onboarding document (pass 4: /projects/new only).
   Like the pass 1-3 documents, a complete self-contained page loading only
   earde.css plus the shared desktop-only mobile gate — no Tailwind, no
   external fonts, no legacy per-page CSS — under the launch app chrome:
   the 54px top bar (brand → /feed, the command field as a styled LINK to
   /search — this wrapper adds no form of its own beyond the page content —
   and viewer-state actions: the anonymous Connect/Log in/Sign up cluster, or the member
   GitHub-mark Connect, the id='notif-badge' bell, and the user chip as a plain link to
   /u/:name), the dark 64px icon rail (Feed, ＋ → /bring; no community tiles —
   the onboarding renderers receive no membership data and none is invented),
   and a centred onboarding column ([container--form]).

   [stepper] is caller-supplied markup rendered INSIDE the column but BEFORE
   the [.create-shell] wrapper: the create-shell opening tag is a byte-exact
   slicing marker for the feature test suites (fragment = create-shell →
   </main>), so all launch chrome — stepper included — must precede it and
   nothing may follow its close inside <main>. [content] is the existing
   feature body, wrapped in a <div class='create-shell'>…</div>, byte-for-byte
   as this route has always emitted it.

   Member documents carry the shared [launch_behavior_script] (the bell badge
   is its only live consumer here — vote selectors match nothing), the same
   behavior this route's pre-launch document had; anonymous documents carry no
   script. Analytics assets are the shared
   [analytics_assets], identical to every other launch document. [page_class]
   (e.g. "launch-project-new") is the scoping root stamped on <body> for the
   route's integration CSS at the end of earde.css. Used only by
   [Project_setup_pages.project_setup_page]. *)
let launch_onboarding_page ?(noindex = false) ?request ?user ?(stepper = "")
    ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  let house_icon =
    "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>"
  in
  let bell_icon =
    "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>"
  in
  let actions =
    match user with
    | Some username ->
        let u = html_escape username in
        let initial =
          if String.length username > 0
          then html_escape (String.sub (String.uppercase_ascii username) 0 1)
          else "?"
        in
        (* The bell's badge is server-rendered from the request's unread count
           and is simply absent at zero (see [Notification_badge]); the user
           chip is the reference markup's plain link — no menu, no extra
           form. *)
        Printf.sprintf
          "%s\
           <a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <a class='userchip' href='/u/%s'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></a>"
          launch_connect_cta bell_icon
          (Notification_badge.badge_html ?request ())
          u initial u
    | None ->
        topbar_anon_actions
  in
  let behavior_script = match user with
    | Some _ -> launch_behavior_script
    | None -> ""
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     %s\n\
     </head>\n\
     <body class='%s'>\n\
     <div class='app'>\n\
     <header class='topbar'>\
     <a class='topbar__brand' href='/feed' aria-label='Earde feed'>\
     <img class='topbar__mark' src='/static/images/logo-mark.svg' alt=''>\
     <img class='topbar__wordmark' src='/static/images/logo-wordmark.svg' alt='Earde'>\
     </a>\
     <a class='search launch-search' href='/search' aria-label='Search Earde'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <span class='launch-search__hint'>grep threads &middot; projects &middot; communities&hellip;</span>\
     <span class='launch-search__enter' aria-hidden='true'>&#8629;</span>\
     </a>\
     <div class='topbar__actions'>%s</div>\
     </header>\n\
     <div class='shell'>\
     <nav class='rail' aria-label='Primary'>\
     <a class='rail__item' href='/feed' title='Feed' aria-label='Feed'>%s</a>\
     <span class='rail__spacer'></span>\
     <a class='rail__item rail__item--add' href='/bring' title='Connect a project' aria-label='Connect a project'>&#65291;</a>\
     </nav>\
     <main class='main'><div class='scroll'><div class='container--form'>\n\
     %s<div class='create-shell'>%s</div></div></div></main>\
     </div>\n\
     %s\n\
     </div>\n\
     %s\n\
     %s\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta mobile_gate_css_link analytics_head
    page_class actions house_icon stepper content launch_footer
    mobile_desktop_gate analytics_banner behavior_script

(* Cartographic Civic launch community document (pass 8: the structured
   /c/:slug overview only). Like the pass 1-7 documents, a complete
   self-contained page loading only earde.css plus the shared desktop-only
   mobile gate — no Tailwind, no external fonts, no legacy per-page CSS —
   but with the approved four-pane community shell: the
   54px top bar (brand → /feed, the REAL /search form, viewer-state actions —
   identical clusters to [launch_app_page], including the member ⋯ menu with
   the existing logout POST form and the id='notif-badge' bell), the dark
   icon rail (Feed, the CURRENT community tile active with the white bleeding
   marker, ＋ → /bring — this renderer receives no joined-community data and
   none is invented), the caller-rendered 244px community [sidebar], and the
   central overview [content]. The overview's right column is page content,
   not the `.aside` pane, so no aside parameter exists.

   Analytics assets carry the §5.3 community group attribute (and the §13
   private marker): the (id, visibility) pair comes from the [community]
   record itself, never from URL shape.
   Replay privacy: for a private community the ph-no-capture class rides on
   the existing `.shell` element (sidebar + main together) — never a new
   wrapper div, so the flex chain is untouched. Member documents carry the
   exact shared [launch_behavior_script] (its vote selectors match nothing
   here); anonymous documents carry no script at all. [page_class]
   ("launch-community-overview") is the scoping root stamped on <body> for
   the route's integration CSS at the end of earde.css. Shared doc builder
   behind [launch_community_page] (pass 8 overview) and
   [launch_community_surface_page] (pass 9 channel), so the two routes'
   global rails can never diverge. *)
let launch_community_doc ?(noindex = false) ?request ?user
    ?(rail_communities = []) ?(head_extra = "") ?(aside = "")
    ~(community : community) ~sidebar ~page_class ~title ~main_el () =
  let analytics_head, analytics_banner =
    analytics_assets ?request
      ~analytics_community:(community.id, community.visibility) ()
  in
  let robots_meta =
    if noindex then "<meta name='robots' content='noindex'>" else ""
  in
  let is_admin = match request with
    | Some req -> (try Dream.session_field req "is_admin" = Some "true" with _ -> false)
    | None -> false
  in
  let house_icon =
    "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>"
  in
  let bell_icon =
    "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>"
  in
  (* Same /search route + ?q= contract (and `required`) as the legacy search
     forms; no /c/:slug test counts page-wide forms, so the real command
     field is safe here (unlike the onboarding wrappers' link variant). *)
  let search_form =
    "<form class='topbar__search-cell' action='/search' method='GET' role='search'>\
     <div class='search'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <label class='sr-only' for='q'>Search Earde</label>\
     <input class='search__input' id='q' type='text' name='q' required placeholder='grep threads &middot; projects &middot; communities&hellip;'>\
     <button class='search__enter' type='submit' aria-label='Search'>&#8629;</button>\
     </div>\
     </form>"
  in
  let actions =
    match user with
    | Some username ->
        let u = html_escape username in
        let admin_item = if is_admin then "<a href='/admin'>Admin</a>" else "" in
        let initial =
          if String.length username > 0
          then html_escape (String.sub (String.uppercase_ascii username) 0 1)
          else "?"
        in
        (* Badge server-rendered from the request's unread count; absent at
           zero (see [Notification_badge]). *)
        Printf.sprintf
          "%s\
           <a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <details class='launch-user'>\
           <summary class='userchip'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></summary>\
           <div class='launch-user__menu'>\
           <a href='/u/%s'>Profile</a>\
           <a href='/settings'>Settings</a>\
           <a href='/notifications'>Notifications</a>\
           %s\
           <form action='/logout' method='POST'><button type='submit'>Log out</button></form>\
           </div>\
           </details>"
          launch_connect_cta bell_icon
          (Notification_badge.badge_html ?request ())
          initial u u admin_item
    | None ->
        topbar_anon_actions
  in
  (* The rail shows only what this renderer really has: Feed, the joined
     communities the handler supplies (kept in their established order), the
     current community, and ＋. Nothing is fabricated: anonymous documents get
     no joined tiles, and the current community is appended only when it is
     not already in the joined run (dedup by slug) — so navigating between
     feed, overview and channel never removes, adds or reorders joined tiles;
     only the active marker moves. *)
  let tile_glyph slug =
    let raw =
      if String.length slug >= 2 then String.sub slug 0 2
      else if String.length slug = 1 then slug
      else "?"
    in
    String.capitalize_ascii raw
  in
  let tile_face_of (c : community) =
    match c.avatar_url with
    | Some url when String.trim url <> "" ->
        let src = safe_img_src url in
        if src = "#" then html_escape (tile_glyph c.slug)
        else Printf.sprintf "<img class='launch-rail__img' src='%s' alt=''>" src
    | _ -> html_escape (tile_glyph c.slug)
  in
  (* One tile per community in the run; the current community carries the
     white active marker wherever it sits (in its joined slot when the viewer
     belongs to it, appended at the end when directly visiting a community
     they haven't joined — an anonymous viewer's rail reduces to exactly the
     current tile). *)
  let rail_tile (c : community) =
    let marker =
      if c.slug = community.slug
      then "<span class='rail__marker rail__marker--community'></span>"
      else ""
    in
    Printf.sprintf
      "<a class='rail__item rail__item--community' href='/c/%s' title='/c/%s' style='background:%s'>%s%s</a>"
      (html_escape c.slug) (html_escape c.slug)
      (launch_tile_color c.slug) marker (tile_face_of c)
  in
  let rail_run =
    if List.exists (fun (c : community) -> c.slug = community.slug) rail_communities
    then rail_communities
    else rail_communities @ [community]
  in
  let rail =
    Printf.sprintf
      "<nav class='rail' aria-label='Primary'>\
       <a class='rail__item' href='/feed' title='Feed' aria-label='Feed'>%s</a>\
       <span class='rail__divider'></span>\
       %s\
       <span class='rail__spacer'></span>\
       <a class='rail__item rail__item--add' href='/bring' title='Connect a project' aria-label='Connect a project'>&#65291;</a>\
       </nav>"
      house_icon
      (String.concat "" (List.map rail_tile rail_run))
  in
  (* Replay privacy on the existing shell element — class only, no wrapper. *)
  let shell_cls =
    if Db.community_is_private community.visibility then "shell ph-no-capture"
    else "shell"
  in
  let behavior_script = match user with
    | Some _ -> launch_behavior_script
    | None -> ""
  in
  Printf.sprintf
    "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s\n\
     %s\n\
     </head>\n\
     <body class='%s'>\n\
     <div class='app'>\n\
     <header class='topbar'>\
     <a class='topbar__brand' href='/feed' aria-label='Earde feed'>\
     <img class='topbar__mark' src='/static/images/logo-mark.svg' alt=''>\
     <img class='topbar__wordmark' src='/static/images/logo-wordmark.svg' alt='Earde'>\
     </a>\
     %s\
     <div class='topbar__actions'>%s</div>\
     </header>\n\
     <div class='%s'>\
     %s\
     %s\
     %s\
     %s\
     </div>\n\
     %s\n\
     </div>\n\
     %s\n\
     %s\n\
     %s\n\
     </body>\n\
     </html>"
    (html_escape title) robots_meta mobile_gate_css_link
    (analytics_head ^ head_extra)
    page_class search_form actions shell_cls rail sidebar main_el aside
    launch_footer mobile_desktop_gate analytics_banner behavior_script

(* Public pass-8 entry point: the overview content is wrapped in the exact
   `<main class='main'>` element (same newlines) the pre-extraction template
   emitted, with no aside. [rail_communities] carries the viewer's joined
   communities so the overview's global rail matches the feed and channel
   routes instead of collapsing to the current tile alone. [head_extra]
   (default absent — every pre-15A caller's output is byte-identical) lets a
   route add a small head fragment; the flat community home uses it to ship
   the guest-only [launch_share_script]. *)
let launch_community_page ?noindex ?request ?user ?rail_communities ?head_extra
    ~(community : community) ~sidebar ~page_class ~title ~content () =
  launch_community_doc ?noindex ?request ?user ?rail_communities ?head_extra
    ~community ~sidebar ~page_class
    ~title ~main_el:("<main class='main'>\n" ^ content ^ "\n</main>") ()

(* Pass-9 channel-safe variant: identical launch chrome (topbar, rail,
   analytics assets, behavior script, mobile gate, tile colours) but the
   caller supplies the COMPLETE prebuilt <main> element — the live channel's
   `<main class='cs-main …'>` flex column must keep its head / stage /
   typing / composer as direct children, which the overview's
   `<main class='main'>` + `.scroll` anatomy would break. [head_extra]
   carries the channel's existing realtime scripts (phoenix.js +
   chat_live.js) and canonical link; [aside] the presence pane;
   [rail_communities] the real joined communities its handler already
   loads. Used only by [Pages.community_channel_shell_page]. *)
let launch_community_surface_page ?noindex ?request ?user ?rail_communities
    ?head_extra ?aside ~(community : community) ~sidebar ~page_class ~title
    ~main_el () =
  launch_community_doc ?noindex ?request ?user ?rail_communities ?head_extra
    ?aside ~community ~sidebar ~page_class ~title ~main_el ()

(* === HELPERS === *)

(* "[deleted_" is set by anonymize_user in Db — both sides must agree on the tombstone format. *)
let is_deleted_user u = String.length u >= 9 && String.sub u 0 9 = "[deleted_"

(* mod_usernames/admin_usernames enable badge rendering at call sites that know the community;
   callers without context omit the params, defaulting to [] so badge logic is a no-op. *)
let render_author ?(mod_usernames=[]) ?(admin_usernames=[]) username =
  if is_deleted_user username then
    "<span class='text-gray-400 italic'>[deleted]</span>"
  else
    let mod_badge =
      if List.mem username mod_usernames then
        "<span class='mod-badge ml-1 text-[10px] font-semibold bg-green-100 text-green-700 px-1.5 py-0.5 rounded'>[MOD]</span>"
      else ""
    in
    (* Admin badge is always site-wide; rendered after MOD so both appear side-by-side
       for the rare case where a site admin is also a local moderator. *)
    let admin_badge =
      if List.mem username admin_usernames then
        "<span class='mod-badge ml-1 text-[10px] font-semibold bg-red-100 text-red-700 px-1.5 py-0.5 rounded'>[ADMIN]</span>"
      else ""
    in
    Printf.sprintf "<a href='/u/%s' class='hover:text-[#C94C4C] hover:underline font-medium transition'>u/%s</a>%s%s" (html_escape username) (html_escape username) mod_badge admin_badge

(* Parse and diff in OCaml rather than casting in SQL to keep DB queries generic
   and avoid timezone drift when the DB and app server are in different locales. *)
let time_ago date_str =
  try
    let clean_str = if String.length date_str >= 19 then String.sub date_str 0 19 else date_str in
    let (y, m, d, h, min, s) =
      Scanf.sscanf clean_str "%d-%d-%d %d:%d:%d" (fun y m d h min s -> (y, m, d, h, min, s))
    in
    let tm = { Unix.tm_sec = s; tm_min = min; tm_hour = h; tm_mday = d;
               tm_mon = m - 1; tm_year = y - 1900; tm_wday = 0; tm_yday = 0; tm_isdst = false } in
    (* mktime treats tm as local time; DB timestamps are UTC.
       Compute UTC offset: mktime(gmtime(now)) returns now interpreted as local → offset = now - mktime(gmtime(now)) *)
    let epoch_local, _ = Unix.mktime tm in
    let now = Unix.gettimeofday () in
    let epoch_gm, _ = Unix.mktime (Unix.gmtime now) in
    let tz_offset = now -. epoch_gm in
    let epoch = epoch_local +. tz_offset in
    let diff = int_of_float (now -. epoch) in

    if diff < 60 then "just now"
    else if diff < 3600 then Printf.sprintf "%d min ago" (diff / 60)
    else if diff < 86400 then Printf.sprintf "%d hr ago" (diff / 3600)
    else if diff < 2592000 then Printf.sprintf "%d days ago" (diff / 86400)
    else if diff < 31536000 then Printf.sprintf "%d mo ago" (diff / 2592000)
    else Printf.sprintf "%d yr ago" (diff / 31536000)
  with _ ->
    date_str

let format_month_year date_str =
  try
    let (y, m) = Scanf.sscanf date_str "%d-%d" (fun y m -> (y, m)) in
    let months = [|"Jan";"Feb";"Mar";"Apr";"May";"Jun";"Jul";"Aug";"Sep";"Oct";"Nov";"Dec"|] in
    if m >= 1 && m <= 12 then Printf.sprintf "%s %d" months.(m-1) y
    else date_str
  with _ -> date_str

(* === CARDS === *)

(* Session reads (current user, is_admin) happen inside a render function to avoid
   threading extra parameters through every call site — coupling is contained here. *)
(* is_current_user_mod and mod_usernames are optional — callers without community context
   (home feed, profile, search) omit them; they default to false/[] so delete
   visibility and author badges behave identically to before on those pages. *)
(* Shared post moderation/admin action markup: own-post Delete (Rule A), Mod/Admin Remove
   dialog (Rule B/C), and Mod/Admin Ban dialog. Lifted verbatim out of render_post so the
   exact same forms, routes, CSRF tags, confirm/dialog behavior, required-reason fields and
   Rule A/B/C visibility can be reused by other surfaces (e.g. search results) with zero
   drift. Returns "" when the viewer has no actionable controls. csrf_token is passed in so
   the caller computes it once and shares it with its other forms. *)
let post_admin_actions ?(is_current_user_mod=false) ?(admin_usernames=[]) ?(banned_usernames=[]) ~csrf_token request (post : post) =
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  (* Tombstone check: both the username prefix (anonymize_user) and content sentinels signal
     a deleted post. Showing a delete button on a tombstone is misleading — the action would
     no-op or error, and could confuse mods into thinking a second deletion is needed. *)
  let is_already_deleted =
    is_deleted_user post.username
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let target_is_admin = List.mem post.username admin_usernames in
  (* Rule A/B/C: strictly mutually exclusive — prevents admins silently using the
     personal Delete path to avoid the public mod log (admin spoofing).
     Rule A: own post → personal Delete. Rule B: mod/top_mod (not own post, not admin-only)
     → Mod Remove dialog. Rule C: admin acting without mod role → Admin Remove dialog. *)
  let action_btn =
    if is_already_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          (* Rule A: personal delete — no audit required *)
          Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0 ml-2' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%d'>
              <button type='submit' class='text-xs text-gray-400 hover:text-red-700 opacity-50 hover:opacity-100 transition'>🗑️</button>
          </form>" csrf_token post.id
        else if is_current_user_mod && not target_is_admin then
          (* Rule B: mod/top_mod removal — dialog enforces public reason *)
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors ml-2'>🛡️ Mod Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-amber-900'>🛡️ Moderator Removal</h3>
                  <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain why this post is being removed (visible to the community)...'
                      class='ph-mask w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else if is_admin && not target_is_admin then
          (* Rule C: admin override (not a mod of this community) — logged as admin_delete_post *)
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors ml-2'>⚡ Admin Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-red-900'>⚡ Admin Intervention</h3>
                  <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain the admin intervention reason (visible to the community)...'
                      class='ph-mask w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else ""
  in

  (* Ban button: mods exile per-community only; suppressed on home/profile feeds where
     is_current_user_mod defaults to false. Never shown for already-deleted accounts.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority
     so an admin who is also a mod doesn't bypass the public mod log via admin path. *)
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username && not (is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            "<span class='text-xs text-red-500 font-semibold ml-2'>🚫 Banned</span>"
          else if is_current_user_mod then
            (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors ml-1'>🔨 Mod Ban</button>
              <dialog id='ban-modal-post-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-amber-900'>🔨 Mod Ban</h3>
                    <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain why this user is being banned (visible to the community)...'
                        class='ph-mask w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-post-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (html_escape post.username) post.community_id post.id
          else
            (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors ml-1'>⚡ Admin Ban</button>
              <dialog id='ban-modal-post-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-red-900'>⚡ Admin Ban</h3>
                    <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain the admin intervention reason (visible to the community)...'
                        class='ph-mask w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-post-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (html_escape post.username) post.community_id post.id
      | _ -> ""
    else ""
  in
  action_btn ^ ban_btn

(* URL slug from a thread title — descriptive only. post_id is the authoritative key in
   /c/:slug/t/:post_id-:post_slug, so a stale or missing slug still resolves (the handler 301s
   to canonical). Same shape as Db's section/community slugify: lowercase, any run of
   non-alphanumerics collapses to a single '-', trimmed, length-capped so URLs stay readable. *)
let slugify title =
  let b = Buffer.create (String.length title) in
  let pending_dash = ref false in   (* defer dashes so leading/collapsed runs never emit one *)
  let any = ref false in
  String.iter (fun c ->
    let lc = Char.lowercase_ascii c in
    if (lc >= 'a' && lc <= 'z') || (lc >= '0' && lc <= '9') then begin
      if !pending_dash && !any then Buffer.add_char b '-';
      pending_dash := false; any := true;
      Buffer.add_char b lc
    end else pending_dash := true)
    title;
  let s = Buffer.contents b in
  (* Cap length, then drop any dash the cut left dangling. *)
  let max_len = 70 in
  let s = if String.length s > max_len then String.sub s 0 max_len else s in
  if String.length s > 0 && s.[String.length s - 1] = '-'
  then String.sub s 0 (String.length s - 1) else s

(* Canonical thread path — single source of truth for the route, the /p redirect, and section
   links. Slug omitted entirely when empty so we never emit a dangling trailing dash. *)
let canonical_thread_path community_slug post_id title =
  let s = slugify title in
  if s = "" then Printf.sprintf "/c/%s/t/%d" community_slug post_id
  else Printf.sprintf "/c/%s/t/%d-%s" community_slug post_id s

(* Destination rendering context for one accepted shared-thread placement
   feed row: the destination community's slug (every internal link must stay
   in the destination context — never eject the reader to the origin URL)
   plus the joined provenance and destination-section fields. [None] is the
   community's own post and renders byte-identically to before. The origin
   name may be linked because the feed queries only return a shared row
   while the origin community is currently public. *)
type feed_shared = string * Db.feed_shared_context

let shared_from_html (post : post) (ctx : Db.feed_shared_context) =
  Printf.sprintf
    "<span class='sth-shared-from'>&#8644; Shared from <a href='/c/%s'>%s</a></span>"
    (html_escape post.community_slug) (html_escape ctx.fs_origin_name)

let render_post ?(is_current_user_mod=false) ?(mod_usernames=[]) ?(admin_usernames=[]) ?(banned_usernames=[]) ?(shared : feed_shared option) request user_votes (post : post) =
  let csrf_token = Dream.csrf_tag request in
  let content_preview = Option.value ~default:"" post.content in
  let link_part = match post.url with | Some u -> Printf.sprintf "<a href='%s' class='text-xs text-[#C94C4C] hover:underline' target='_blank'>%s ↗</a>" (safe_url u) (html_escape u) | None -> "" in
  (* Shared rows link into the DESTINATION thread context (server-built from
     the destination slug and canonical post data — no stored URL); the
     community's own rows keep the legacy /p/:id links byte-for-byte. *)
  let thread_target = match shared with
    | Some (destination_slug, _) ->
        canonical_thread_path destination_slug post.id post.title
    | None -> Printf.sprintf "/p/%d" post.id in

  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in

  let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in

  let current_user = Dream.session_field request "username" in
  (* Mod/admin action markup (own-post Delete, Mod/Admin Remove, Mod/Admin Ban) is built by
     the shared post_admin_actions helper so search results reuse the exact same forms.
     On a SHARED row the destination's moderator standing grants nothing over canonical
     content, so the mod arm is dropped (and with it the destination-scoped ban list,
     which pairs with the wrong community): only the author's own delete and the global
     admin's origin-scoped controls remain. *)
  let admin_actions =
    match shared with
    | None ->
        post_admin_actions ~is_current_user_mod ~admin_usernames ~banned_usernames ~csrf_token request post
    | Some _ ->
        post_admin_actions ~is_current_user_mod:false ~admin_usernames ~banned_usernames:[] ~csrf_token request post
  in

  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▲</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 font-bold text-sm leading-none'>▲</a>"
  in
  let downvote_html =
    if not post.allow_downvotes then ""
    else match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▼</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] font-bold text-sm leading-none'>▼</a>"
  in

  (* Image thumbnail: capped at 320 px wide in the card — full resolution served from
     static/uploads/; no second resize needed because the file is already ≤1920x1080. *)
  let image_html = match post.image_url with
    | None -> ""
    | Some img -> Printf.sprintf "<a href='%s' class='block mt-2 mb-1'><img src='%s' alt='Post image' class='w-full max-h-[512px] object-contain bg-stone-900 rounded-lg border border-[#E0D9CC]'></a>"
        (html_escape thread_target) (html_escape img)
  in
  (* Meta head: the community's own rows keep the /c/:slug chip and their
     origin-section chip byte-for-byte. A shared row instead leads with the
     "Shared from <origin>" provenance and shows the placement's DESTINATION
     section — the effective local context — never the origin's. *)
  let context_chip = match shared with
    | None ->
        Printf.sprintf "<a href='/c/%s' class='font-semibold text-gray-700 hover:text-[#C94C4C] transition relative z-10'>/c/%s</a>"
          (html_escape post.community_slug) (html_escape post.community_slug)
    | Some (_, ctx) -> shared_from_html post ctx
  in
  let section_chip = match shared with
    | None ->
        (match post.section_name, post.section_slug with
         | Some sn, Some ss ->
             Printf.sprintf "<span class='text-gray-300'>›</span><a href='/c/%s/s/%s' class='font-medium text-[#C94C4C] hover:underline transition relative z-10'>%s</a>"
               (html_escape post.community_slug) (html_escape ss) (html_escape sn)
         | _ when post.community_sections_enabled ->
             Printf.sprintf "<span class='text-gray-300'>›</span><a href='/c/%s/s/uncategorized' class='font-medium text-[#C94C4C] hover:underline transition relative z-10'>Uncategorized</a>"
               (html_escape post.community_slug)
         | _ -> "")
    | Some (destination_slug, ctx) ->
        (match ctx.fs_section_name, ctx.fs_section_slug with
         | Some sn, Some ss ->
             Printf.sprintf "<span class='text-gray-300'>›</span><a href='/c/%s/s/%s' class='font-medium text-[#C94C4C] hover:underline transition relative z-10'>%s</a>"
               (html_escape destination_slug) (html_escape ss) (html_escape sn)
         | _ -> "")
  in
  Printf.sprintf "
  <div onclick=\"if(!event.target.closest('a, button, form')) window.location='%s'\" class='cursor-pointer border-b border-[#E8E2D9] py-4 flex gap-4 hover:bg-[#F0EBE0] transition-colors'>

      <div class='flex flex-col items-center pt-0.5 w-7 shrink-0 cursor-default' onclick=\"event.stopPropagation()\">
          %s
          <span class='font-semibold text-gray-600 text-xs my-0.5'>%d</span>
          %s
      </div>

      <div class='flex-1 min-w-0'>
          <div class='flex flex-wrap items-center gap-x-1.5 text-xs text-gray-500 mb-1'>
              %s
              %s
              <span class='text-gray-300'>•</span>
              <span>by</span>
              <span class='relative z-10'>%s</span>
              <span class='text-gray-300'>•</span>
              <span>%s</span>
              <span class='relative z-10'>%s</span>
          </div>

          <h3 class='text-base font-semibold text-gray-900 leading-snug mb-1'>
              <a href='%s' class='ph-mask hover:text-[#C94C4C] transition break-words'>%s</a>
          </h3>

          <div class='relative z-10 text-xs'>%s</div>
          %s
          <p class='ph-mask text-sm text-gray-600 mt-2 break-words line-clamp-6'>%s</p>

          <div class='flex items-center mt-2 text-xs text-gray-400'>
              <a href='%s' class='hover:text-[#C94C4C] flex items-center gap-1 transition relative z-10'>
                  <span>💬</span><span>%d comments</span>
              </a>
              <button type='button' onclick='copyPostLink(\"%s\", this)' class='text-xs font-medium text-gray-500 hover:text-gray-900 flex items-center transition-colors cursor-pointer ml-4'>🔗 Share</button>
          </div>
      </div>
  </div>"
  (html_escape thread_target)
  upvote_html post.score downvote_html
  context_chip
  section_chip
  (render_author ~mod_usernames ~admin_usernames post.username) (time_ago post.created_at) admin_actions
  (html_escape thread_target) (html_escape post.title) link_part image_html (html_escape content_preview) (html_escape thread_target) post.comment_count (html_escape thread_target)

(* Compact host for a link post's domain chip: strip scheme + a leading www. and cut at the
   first path/query/fragment. None when it doesn't look like an http(s) URL, so the row shows
   no chip rather than a misleading fragment. Display goes through html_escape; the href uses
   safe_url. Hand-rolled (no Uri dep on the hot render path) — only needs host extraction. *)
let extract_domain url =
  let u = String.trim url in
  let strip_prefix p s =
    let lp = String.length p in
    if String.length s >= lp && String.lowercase_ascii (String.sub s 0 lp) = p
    then Some (String.sub s lp (String.length s - lp)) else None
  in
  match (match strip_prefix "https://" u with Some r -> Some r | None -> strip_prefix "http://" u) with
  | None -> None
  | Some rest ->
      let host_end =
        match List.filter_map (fun c -> String.index_opt rest c) ['/'; '?'; '#'] with
        | [] -> String.length rest
        | l -> List.fold_left min (String.length rest) l
      in
      let host = String.sub rest 0 host_end in
      let host = match strip_prefix "www." host with Some h -> h | None -> host in
      if host = "" then None else Some host

(* The section row's ⋯ menu reuses the EXACT moderation UI render_post builds — identical routes,
   CSRF, dialog ids and Rule A/B/C permission logic — so the forum feed can't drift from the
   warm-card feed. render_post is deliberately NOT refactored to call this; it keeps its own inline
   copy so warm-card pages stay byte-for-byte unchanged (a conservative duplication, by request).
   Returns "" when the viewer has no available action (anon, or a tombstoned / admin-protected
   target) so the caller can omit the menu entirely. Dialog ids are keyed by post.id and never
   collide with render_post because the two renderers never appear on the same page. *)
let mod_action_controls ~is_current_user_mod ~admin_usernames ~banned_usernames request (post : post) =
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let is_already_deleted =
    is_deleted_user post.username
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let target_is_admin = List.mem post.username admin_usernames in
  (* Rule A: own post → personal Delete (labelled in the menu, route/CSRF identical to the card).
     Rule B: mod/top_mod → Mod Remove dialog. Rule C: admin without mod role → Admin Remove. *)
  let action_btn =
    if is_already_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          Printf.sprintf "<form action='/delete-post' method='POST' class='m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%d'>
              <button type='submit' class='text-xs font-bold text-gray-600 hover:text-red-700 border border-gray-300 bg-gray-50 hover:bg-red-50 rounded px-2 py-0.5 transition-colors'>🗑️ Delete</button>
          </form>" csrf_token post.id
        else if is_current_user_mod && not target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🛡️ Mod Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-amber-900'>🛡️ Moderator Removal</h3>
                  <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain why this post is being removed (visible to the community)...'
                      class='ph-mask w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else if is_admin && not target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-red-900'>⚡ Admin Intervention</h3>
                  <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain the admin intervention reason (visible to the community)...'
                      class='ph-mask w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                  </label>
                  <div class='flex justify-end gap-2 pt-1'>
                    <button type='button' onclick=\"document.getElementById('mod-modal-%d').close()\"
                      class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                    <button type='submit'
                      class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                  </div>
                </form>
              </div>
            </dialog>"
            post.id post.id post.community_slug post.id csrf_token post.id
        else ""
  in
  (* Ban: mods exile per-community; admins (without mod role) act via the override path. Mutually
     exclusive with the mod path so an admin who is also a mod still goes through the public mod
     log. A banned account shows a badge instead of a button to prevent double-ban confusion. *)
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username && not (is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            "<span class='text-xs text-red-500 font-semibold'>🚫 Banned</span>"
          else if is_current_user_mod then
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🔨 Mod Ban</button>
              <dialog id='ban-modal-post-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-amber-900'>🔨 Mod Ban</h3>
                    <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain why this user is being banned (visible to the community)...'
                        class='ph-mask w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-post-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (html_escape post.username) post.community_id post.id
          else
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Ban</button>
              <dialog id='ban-modal-post-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-red-900'>⚡ Admin Ban</h3>
                    <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                  </div>
                  <form action='/ban-community-user' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                    %s
                    <input type='hidden' name='target_username' value='%s'>
                    <input type='hidden' name='community_id' value='%d'>
                    <label class='flex flex-col gap-1.5'>
                      <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                      <textarea name='reason' required rows='4'
                        placeholder='Explain the admin intervention reason (visible to the community)...'
                        class='ph-mask w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-post-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (html_escape post.username) post.community_id post.id
      | _ -> ""
    else ""
  in
  action_btn ^ ban_btn

(* Section-feed thread row — the cool-grey, thread-first counterpart to render_post (warm card).
   Used only by community_section_shell_page. Vote markup is reused verbatim from the card so the
   optimistic-vote JS (see layout) keeps working: the .cs-vote control's only direct children are
   the upvote form, the score <span>, and the downvote form, with the same Tailwind colour classes
   the JS toggles. The body is intentionally lighter than the card: strong title, an optional
   single-line preview, a compact monospace meta row, and moderation tucked into a ⋯ menu. *)
let render_forum_row ?(is_current_user_mod=false) ?(mod_usernames=[]) ?(admin_usernames=[]) ?(banned_usernames=[]) ?(show_context=false) ?(shared : feed_shared option) request user_votes (post : post) =
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in
  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in
  let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in
  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▲</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 font-bold text-sm leading-none'>▲</a>"
  in
  let downvote_html =
    if not post.allow_downvotes then ""
    else match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▼</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] font-bold text-sm leading-none'>▼</a>"
  in
  (* Domain chip: only for link posts with a parseable host; external link opens in a new tab. *)
  let domain_html = match post.url with
    | Some u -> (match extract_domain u with
        | Some d -> Printf.sprintf "<a class='ft-domain' href='%s' target='_blank' rel='noopener'>%s ↗</a>" (safe_url u) (html_escape d)
        | None -> "")
    | None -> ""
  in
  (* Secondary one-line preview: only when there's real body text (CSS clamps to a single line).
     Omitted entirely for link-only / empty posts — no placeholder. *)
  let preview_html = match post.content with
    | Some c when String.trim c <> "" -> Printf.sprintf "<div class='ft-preview'>%s</div>" (html_escape c)
    | _ -> ""
  in
  (* ⋯ menu rendered only when the viewer actually has an action (keeps the feed clean, not admin-y).
     On a SHARED row the destination's moderator standing grants nothing over canonical content:
     the mod arm is dropped (with its destination-scoped ban list, which pairs with the wrong
     community) — only own-post delete and global-admin origin-scoped controls survive. *)
  let mod_controls =
    match shared with
    | None -> mod_action_controls ~is_current_user_mod ~admin_usernames ~banned_usernames request post
    | Some _ -> mod_action_controls ~is_current_user_mod:false ~admin_usernames ~banned_usernames:[] request post
  in
  let mod_menu =
    if mod_controls = "" then ""
    else Printf.sprintf "<details class='cs-row-mod'><summary>⋯</summary><div class='cs-row-mod-menu'>%s</div></details>" mod_controls
  in
  (* Internal links point at the canonical thread URL, not legacy /p/:id (which now 301s here).
     A shared row links into the DESTINATION thread context instead — the reader stays in the
     community they are browsing; the path is server-built from the destination slug and the
     canonical post, never a stored URL. *)
  let thread_href = match shared with
    | Some (destination_slug, _) -> canonical_thread_path destination_slug post.id post.title
    | None -> canonical_thread_path post.community_slug post.id post.title
  in
  (* show_context is set by the global Feed, where rows span communities/sections, so each row
     names its own origin: "§ Section · /c/slug". Section pages leave it off — the page header
     already supplies that context — so those views stay byte-for-byte unchanged. A shared row
     instead always carries its one compact provenance line. *)
  let context_html =
    match shared with
    | Some (_, ctx) -> Printf.sprintf "<div class='ft-ctx'>%s</div>" (shared_from_html post ctx)
    | None ->
    if not show_context then ""
    else
      let section_html = match post.section_name, post.section_slug with
        | Some name, Some slug when String.trim name <> "" ->
            Printf.sprintf "<span class='sec'>§</span> <a href='/c/%s/s/%s'>%s</a> <span class='ft-ctx-dot'>·</span> "
              (html_escape post.community_slug) (html_escape slug) (html_escape name)
        | _ -> ""
      in
      Printf.sprintf "<div class='ft-ctx'>%s<a class='ft-ctx-c' href='/c/%s'>/c/%s</a></div>"
        section_html (html_escape post.community_slug) (html_escape post.community_slug)
  in
  Printf.sprintf "
  <div class='cs-thread'>
      <div class='cs-vote'>%s<span class='cs-vote-score'>%d</span>%s</div>
      <div class='ft-main'>
          %s
          <div class='ft-title'><a href='%s'>%s</a></div>
          %s
          <div class='ft-meta'>%s<span>by %s</span><span>%s</span><a href='%s'>💬 %d</a></div>
      </div>
      %s
  </div>"
    upvote_html post.score downvote_html
    context_html
    thread_href (html_escape post.title)
    preview_html
    domain_html (render_author ~mod_usernames ~admin_usernames post.username) (time_ago post.created_at) thread_href post.comment_count
    mod_menu

(* === REPLAY PRIVACY ===
   The multi-pane legacy shell (global_shell / community_shell / feed_shell,
   their nav_item/nav_group/rail_active types and renderers) and the focused
   single-column wrappers (create/account/admin/community-manage/
   community-home/search) are gone with their per-page stylesheets; the
   launch_* documents above own every live surface. Only the replay guard,
   which the launch community pages still call, survives here. *)

(* Replay privacy (analytics spec §6): private-community page content is
   blocked from session replay entirely via PostHog's built-in ph-no-capture
   class, on top of the selector-based text masking. *)
let private_replay_guard ~(community : Db.community) body =
  if community.Db.visibility = Db.Community_private then
    "<div class='ph-no-capture'>" ^ body ^ "</div>"
  else body
