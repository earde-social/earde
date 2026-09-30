open Html.Infix

(* === LAYOUT ===
   Every live document is a complete, self-contained Cartographic Civic
   launch page (the launch_* wrappers below). They share exactly two
   ingredients — the desktop-only mobile gate and the analytics assets —
   and load no external stylesheet, font or script. *)

(* Desktop-only gate. MVP is desktop-first: rather than making the multi-pane app shell
   responsive, a phone-width viewport gets a centered "open on desktop" panel instead of the
   app. Pure CSS + SSR — no JS, no user-agent sniffing, no separate route. mobile-gate.css hides
   the app chrome and reveals this panel under the breakpoint; on wide screens the panel is
   display:none and nothing changes. Injected on `App surfaces (see layout) and, opt-in via
   [launch_entry_page ~desktop_only:true], on /bring — the project-onboarding funnel is a
   desktop flow, so a phone-width visitor gets the gate instead of an entry point into it.
   Auth/legal/marketing pages (including /privacy) are left usable. The HTML always renders,
   so crawlers/SSR are unaffected. *)
let mobile_gate_css_link = (Html.static "<link rel='stylesheet' href='/static/css/mobile-gate.css'>")

let mobile_desktop_gate =
  (Html.static "<div class='mobile-gate' role='dialog' aria-label='Desktop only'>\
     <div class='mobile-gate-card'>\
       <div class='mobile-gate-brand'><img src='/static/images/logo-mark.svg' alt='' class='mobile-gate-logo-mark'><img src='/static/images/logo-wordmark.svg' alt='Earde' class='mobile-gate-logo-wordmark'></div>\
       <h1 class='mobile-gate-title'>Desktop only for now</h1>\
       <p class='mobile-gate-body'>Earde is currently built for laptop and desktop screens. Open this page on a larger screen to use the full app.</p>\
       <p class='mobile-gate-note'>Mobile support is coming later.</p>\
     </div>\
   </div>")

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
  match Analytics.browser_config () with
    | None -> (Html.empty, Html.empty)
    | Some cfg ->
        let identity_attr =
          match request with
          | None -> Html.empty
          | Some req -> (
              (* Absence of session middleware (tests) or a malformed session
                 value must safely mean "no identity". *)
              match (try Dream.session_field req "user_id" with _ -> None) with
              | None -> Html.empty
              | Some uid_str -> (
                  match int_of_string_opt uid_str with
                  | Some id ->
                      Html.template " data-analytics-user='%s'"
                        [ Html.text (Analytics.distinct_id_of_user_id id) ]
                  | None -> Html.empty))
        in
        (* Community context arrives as (id, authoritative visibility) — the
           pair comes from the Community_types.community record the page already holds, so
           the §13 private marker can never be derived from URL shape and a
           call site cannot pass the id without stating visibility. The
           marker value is only "true": no name, no slug. On marked
           documents analytics.js records consent but never loads the SDK. *)
        let group_attr, private_attr =
          match analytics_community with
          | Some (community_id, visibility) ->
              ( Html.template " data-analytics-group='%s'"
                  [ Html.text (Analytics.community_group_key community_id) ],
                if Community_types.community_is_private visibility then
                  (Html.static " data-analytics-private-community='true'")
                else Html.empty )
          | None -> (Html.empty, Html.empty)
        in
        ( (Html.static "<script src='/static/js/analytics.js' defer></script>"),
          Html.template
            "<div id='analytics-consent' hidden data-ph-token='%s' data-ph-api-host='%s' data-ph-deployment-environment='%s'%s%s%s class='fixed bottom-4 left-1/2 -translate-x-1/2 z-50 w-[calc(100%-2rem)] max-w-md bg-white border border-[#E0D9CC] rounded-2xl shadow-xl p-4'>\
               <p class='text-sm text-gray-700 mb-3'>Earde can collect optional usage analytics (PostHog) to improve the product. Nothing is collected until you choose, and you can change your choice at any time on the <a href='/privacy#cookies-analytics'>privacy page</a>.</p>\
               <div class='flex items-center gap-2'>\
                 <button type='button' data-analytics-accept class='px-4 py-1.5 text-sm font-semibold bg-[#C94C4C] text-white rounded-full hover:bg-[#A83A3A] transition'>Accept</button>\
                 <button type='button' data-analytics-refuse class='px-4 py-1.5 text-sm font-semibold text-gray-600 bg-gray-100 rounded-full hover:bg-gray-200 transition'>Refuse</button>\
                 <span data-analytics-error hidden class='text-xs text-red-600'>Couldn&#39;t save &mdash; try again.</span>\
               </div>\
             </div>"
            [ Html.text cfg.Analytics.browser_token
            ; Html.text cfg.Analytics.browser_api_host
            ; Html.text cfg.Analytics.browser_deployment_environment
            ; identity_attr; group_attr; private_attr ] )

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
  (Html.static "<a class='btn btn--connect-github' href='/bring' \
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
   <span>Connect a project</span></a>")

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
  (Html.static "<footer class='launch-footer'>\
   <a href='/privacy'>Privacy</a>\
   <span class='launch-footer__sep' aria-hidden='true'>&middot;</span>\
   <a href='/privacy#cookies-analytics'>Analytics preferences</a>\
   </footer>")

(* The one anonymous topbar action cluster: the shared Connect CTA plus the
   two authentication controls. One value used by every launch wrapper with
   an anonymous arm, so the anonymous and authenticated topbars render the
   byte-identical CTA and only the surrounding controls differ. *)
let topbar_anon_actions =
  launch_connect_cta
  ++ (Html.static "<a class='btn btn--secondary btn--auth' href='/login'>Log in</a>\
     <a class='btn btn--primary btn--auth' href='/signup'>Sign up</a>")

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

(* Cartographic Civic launch entry document (/bring). A complete,
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
   that integration CSS (e.g. "launch-bring"), stamped on <body>.

   [desktop_only] (default false) opts the document into the shared desktop-only gate — the
   same [mobile_gate_css_link] + [mobile_desktop_gate] pair the `App wrappers ship, plus a
   per-route `body.<page_class> > .app` hide rule in earde.css. /bring sets it because the
   project-onboarding funnel it opens is desktop-only; /privacy leaves it off and stays
   readable at every width, byte-identically to before this option existed. *)
let launch_entry_page ?(noindex = false) ?request ?(topbar = Entry_connect_cta)
    ?(desktop_only = false) ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then (Html.static "<meta name='robots' content='noindex'>") else Html.empty
  in
  (* Both halves of the gate carry their own trailing newline, so a document
     that opts out is byte-identical to the pre-option rendering. *)
  let gate_css = if desktop_only then mobile_gate_css_link ++ (Html.static "\n") else Html.empty in
  let gate_panel = if desktop_only then mobile_desktop_gate ++ (Html.static "\n") else Html.empty in
  let house_icon =
    (Html.static "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>")
  in
  let bell_icon =
    (Html.static "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>")
  in
  let actions =
    match topbar with
    | Entry_connect_cta -> launch_connect_cta
    | Entry_viewer (Some username) ->
        let u = (Html.text (username)) in
        let initial =
          if String.length username > 0
          then (Html.text ((String.sub (String.uppercase_ascii username) 0 1)))
          else (Html.static "?")
        in
        (* Form-free member cluster (the onboarding wrapper's, minus the CTA):
           badge server-rendered from the request's unread count, user chip a
           plain link — no <details> menu, no logout form. *)
        (Html.template "<a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <a class='userchip' href='/u/%s'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></a>"
  [ bell_icon
  ; (Notification_badge.badge_html ?request ())
  ; u
  ; initial
  ; u ])
    | Entry_viewer None ->
        (Html.static "<a class='btn btn--secondary btn--auth' href='/login'>Log in</a>\
         <a class='btn btn--primary btn--auth' href='/signup'>Sign up</a>")
  in
  Html.to_string (Html.template "<!DOCTYPE html>\n\
     <html lang='en'>\n\
     <head>\n\
     <meta charset='UTF-8'>\n\
     <meta name='viewport' content='width=device-width, initial-scale=1.0'>\n\
     <title>%s - Earde</title>\n\
     %s\n\
     <link rel='stylesheet' href='/static/css/earde.css'>\n\
     %s%s\n\
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
     %s%s\n\
     </body>\n\
     </html>"
  [ (Html.text (title))
  ; robots_meta
  ; gate_css
  ; analytics_head
  ; Html.text page_class
  ; actions
  ; house_icon
  ; content
  ; launch_footer
  ; gate_panel
  ; analytics_banner ])

(* Cartographic Civic launch auth document (/login and /signup).
   Like [launch_entry_page], a complete self-contained document that loads only
   earde.css — no Tailwind, no external fonts, no legacy per-page CSS, no
   mobile gate, no notification polling — but with the approved *anonymous* top bar (the shared
   Connect-a-project CTA / Log in / Sign up) instead of the entry chrome, and no icon rail:
   the /login and /signup routes render no panes at all (04-ROUTES).

   The chrome is deterministic and viewer-independent: the same three real
   routes for every request, no bell, no user chip, no logout form. The
   command field stays a styled link to /search (same trick as the entry document) so this
   wrapper adds no form contract beyond the page's own auth form. Analytics
   behavior is the shared [analytics_assets], identical to
   [launch_entry_page]. [page_class] ("launch-login" / "launch-signup") lands
   on <body> next to the shared "launch-auth" scope root that the integration
   CSS at the end of earde.css keys on. Existing wrappers and their callers
   are untouched. *)
let launch_auth_page ?(noindex = false) ?request ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then (Html.static "<meta name='robots' content='noindex'>") else Html.empty
  in
  Html.to_string (Html.template "<!DOCTYPE html>\n\
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
  [ (Html.text (title))
  ; robots_meta
  ; analytics_head
  ; Html.text page_class
  ; topbar_anon_actions
  ; content
  ; launch_footer
  ; analytics_banner ])

(* Cartographic Civic launch message document (the shared
   Site_pages.msg_page). Like the other launch documents, complete and
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
    if noindex then (Html.static "<meta name='robots' content='noindex'>") else Html.empty
  in
  Html.to_string (Html.template "<!DOCTYPE html>\n\
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
  [ (Html.text (title))
  ; robots_meta
  ; analytics_head
  ; content
  ; analytics_banner ])

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
let launch_share_snippet = (Html.static {js|        /* Clipboard write is async; we optimistically swap innerHTML and class list
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
        }|js})

(* Guest-only public-interaction script: exactly the shared copyPostLink and
   nothing else — no confirm modal and no vote handler (anonymous documents
   must fire zero requests beyond assets). Emitted per route (currently the flat community home,
   whose byte-pinned rows show Share to every viewer); the doc builders'
   anonymous default stays script-free. *)
let launch_share_script = (Html.static "<script>\n") ++ launch_share_snippet ++ (Html.static "\n      </script>")

let launch_behavior_script = (Html.static {js|<script>
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
|js}) ++ launch_share_snippet ++ (Html.static {js|
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
      </script>|js})

(* Cartographic Civic launch app document (/feed). Like the entry and
   auth documents, a complete self-contained page loading only earde.css — no
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
    ?analytics_community ?(aside = Html.empty) ~page_class ~title ~content () =
  let analytics_head, analytics_banner =
    analytics_assets ?request ?analytics_community ()
  in
  let robots_meta =
    if noindex then (Html.static "<meta name='robots' content='noindex'>") else Html.empty
  in
  let is_admin = match request with
    | Some req -> (try Dream.session_field req "is_admin" = Some "true" with _ -> false)
    | None -> false
  in
  let house_icon =
    (Html.static "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>")
  in
  let bell_icon =
    (Html.static "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>")
  in
  (* Same /search route + ?q= contract (and `required`) as both legacy search
     forms; only the skin is the handoff command field. *)
  let search_form =
    (Html.static "<form class='topbar__search-cell' action='/search' method='GET' role='search'>\
     <div class='search'>\
     <span class='search__sigil' aria-hidden='true'>/</span>\
     <label class='sr-only' for='q'>Search Earde</label>\
     <input class='search__input' id='q' type='text' name='q' required placeholder='grep threads &middot; projects &middot; communities&hellip;'>\
     <button class='search__enter' type='submit' aria-label='Search'>&#8629;</button>\
     </div>\
     </form>")
  in
  let actions =
    match user with
    | Some username ->
        let u = (Html.text (username)) in
        (* /admin is admin-only; the handler re-checks is_admin, so offering
           the link to a flagged session leaks nothing. *)
        let admin_item = if is_admin then (Html.static "<a href='/admin'>Admin</a>") else Html.empty in
        let initial =
          if String.length username > 0
          then (Html.text ((String.sub (String.uppercase_ascii username) 0 1)))
          else (Html.static "?")
        in
        (* User menu is a pure-CSS <details>; Log out stays a POST form with
           its existing action/semantics. The bell's badge is server-rendered
           from the request's unread count and is simply absent at zero — see
           [Notification_badge]. *)
        (Html.template "%s\
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
  [ launch_connect_cta
  ; bell_icon
  ; (Notification_badge.badge_html ?request ())
  ; initial
  ; u
  ; u
  ; admin_item ])
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
      (fun (c : Community_types.community) ->
        let face =
          match c.avatar_url with
          | Some url -> (
              match Html.image_src_opt url with
              | Some src ->
                  Html.template "<img class='launch-rail__img' src='%s' alt=''>" [ src ]
              | None -> Html.text (tile_glyph c.slug))
          | None -> Html.text (tile_glyph c.slug)
        in
        (Html.template "<a class='rail__item rail__item--community' href='/c/%s/ch/general' title='/c/%s' style='background:%s'>%s</a>"
  [ (Html.text (c.slug))
  ; (Html.text (c.slug))
  ; (Html.text (launch_tile_color c.slug))
  ; face ]))
      rail_communities
  in
  let divider =
    if rail_communities = [] then Html.empty else (Html.static "<span class='rail__divider'></span>")
  in
  let rail =
    (Html.template "<nav class='rail' aria-label='Primary'>\
       <a class='rail__item' href='/feed' title='Feed' aria-label='Feed'><span class='rail__marker'></span>%s</a>\
       %s%s\
       <span class='rail__spacer'></span>\
       <a class='rail__item rail__item--add' href='/bring' title='Connect a project' aria-label='Connect a project'>&#65291;</a>\
       </nav>"
  [ house_icon
  ; divider
  ; (Html.concat tiles) ])
  in
  let aside_html =
    if Html.is_empty aside then Html.empty
    else (Html.template "<aside class='aside' aria-label='Secondary'>%s</aside>"
  [ aside ])
  in
  let behavior_script = match user with
    | Some _ -> launch_behavior_script
    | None -> Html.empty
  in
  Html.to_string (Html.template "<!DOCTYPE html>\n\
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
  [ (Html.text (title))
  ; robots_meta
  ; mobile_gate_css_link
  ; analytics_head
  ; Html.text page_class
  ; search_form
  ; actions
  ; rail
  ; content
  ; aside_html
  ; launch_footer
  ; mobile_desktop_gate
  ; analytics_banner
  ; behavior_script ])

(* Cartographic Civic launch onboarding document (/projects/new).
   Like the other launch documents, a complete self-contained page loading only
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
let launch_onboarding_page ?(noindex = false) ?request ?user ?(stepper = Html.empty)
    ~page_class ~title ~content () =
  let analytics_head, analytics_banner = analytics_assets ?request () in
  let robots_meta =
    if noindex then (Html.static "<meta name='robots' content='noindex'>") else Html.empty
  in
  let house_icon =
    (Html.static "<svg width='18' height='18' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M3 10.5 12 3l9 \
     7.5'></path><path d='M5 9.5V21h14V9.5'></path></svg>")
  in
  let bell_icon =
    (Html.static "<svg width='16' height='16' viewBox='0 0 24 24' fill='none' \
     stroke='currentColor' stroke-width='1.7' stroke-linecap='round' \
     stroke-linejoin='round' aria-hidden='true'><path d='M18 8a6 6 0 0 \
     0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg>")
  in
  let actions =
    match user with
    | Some username ->
        let u = (Html.text (username)) in
        let initial =
          if String.length username > 0
          then (Html.text ((String.sub (String.uppercase_ascii username) 0 1)))
          else (Html.static "?")
        in
        (* The bell's badge is server-rendered from the request's unread count
           and is simply absent at zero (see [Notification_badge]); the user
           chip is the reference markup's plain link — no menu, no extra
           form. *)
        (Html.template "%s\
           <a class='bell' href='/notifications' title='Notifications' aria-label='Notifications'>%s%s</a>\
           <a class='userchip' href='/u/%s'><span class='avatar avatar--24'>%s</span><span class='userchip__name'>u/%s</span></a>"
  [ launch_connect_cta
  ; bell_icon
  ; (Notification_badge.badge_html ?request ())
  ; u
  ; initial
  ; u ])
    | None ->
        topbar_anon_actions
  in
  let behavior_script = match user with
    | Some _ -> launch_behavior_script
    | None -> Html.empty
  in
  Html.to_string (Html.template "<!DOCTYPE html>\n\
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
  [ (Html.text (title))
  ; robots_meta
  ; mobile_gate_css_link
  ; analytics_head
  ; Html.text page_class
  ; actions
  ; house_icon
  ; stepper
  ; content
  ; launch_footer
  ; mobile_desktop_gate
  ; analytics_banner
  ; behavior_script ])

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
let private_replay_guard ~(community : Community_types.community) body =
  if community.Community_types.visibility = Community_types.Community_private then
    Html.static "<div class='ph-no-capture'>" ++ body ++ Html.static "</div>"
  else body
