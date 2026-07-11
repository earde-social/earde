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
      else Printf.sprintf "<img src='%s' class='%s' alt='%s' loading='lazy' decoding='async'>" src img_class (html_escape alt)
  | _ -> initial_tile ~class_:tile_class username

(* Community counterpart of user_avatar; the tile glyph is the community name's first letter. *)
let community_avatar ?(alt="") ~img_class ~tile_class ~name avatar_url =
  match avatar_url with
  | Some url when String.trim url <> "" ->
      let src = safe_img_src url in
      if src = "#" then initial_tile ~class_:tile_class name
      else Printf.sprintf "<img src='%s' class='%s' alt='%s' loading='lazy' decoding='async'>" src img_class (html_escape alt)
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
      else Printf.sprintf "<img src='%s' class='%s' alt='%s' loading='lazy' decoding='async'>" src img_class (html_escape alt)
  | _ -> ""

(* === LAYOUT === *)

(* request is optional so callers without session context (e.g. choose_community_page)
   can omit it and get is_admin=false by default — no forced parameter threading. *)
(* head_extra is opt-in per page so feature pages — e.g. the community shell — can pull
   in their own stylesheet without affecting any other call site. *)

(* The App command bar: the mono, app-like topbar used by every shell surface (feed + community
   channel/section/thread). It is deliberately distinct from the warm Site navbar so the in-app
   shell reads as one coherent product instead of half-old-site. Markup mirrors the mockup's
   .topbar; styles live in shell.css (.app-topbar…), which is loaded only on shell pages, so these
   unscoped class names never leak to Site pages. Uses only real routes — no fake links. *)
let render_app_topbar ?user ?request:_ ~is_admin () =
  let nav = "" in
  (* Same /search route + ?q= contract as the Site search; only the styling is grep-like. *)
  let search =
    "<form class='app-search' action='/search' method='GET'>\
       <span class='app-sigil'>/</span>\
       <input type='text' name='q' required placeholder='grep public threads · communities…' aria-label='Search threads and communities'>\
       <button type='submit' title='Search' aria-label='Search'>&#8629;</button>\
     </form>"
  in
  let right =
    match user with
    | Some username ->
        let u = html_escape username in
        (* /admin is admin-only; the handler re-checks is_admin, so the link leaks nothing. There is
           no global mod dashboard route, so no Mod link is offered (no fake links). *)
        let admin_item = if is_admin then "<a href='/admin'>Admin</a>" else "" in
        let initial =
          if String.length username > 0
          then html_escape (String.sub (String.uppercase_ascii username) 0 1)
          else "?"
        in
        (* User menu is a pure-CSS <details> (no JS), modeled on the existing .cs-row-mod menu.
           Log out stays a POST form — unchanged route semantics. The notifications link keeps
           id='notif-badge' exactly where the layout's polling JS expects it. *)
        Printf.sprintf "
          <a class='app-start' href='/new-community'>+ Start community</a>
          <a class='app-bell' href='/notifications' title='Notifications' aria-label='Notifications'><svg class='app-icon' aria-hidden='true' viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='1.7' stroke-linecap='round' stroke-linejoin='round'><path d='M18 8a6 6 0 0 0-12 0c0 7-3 7-3 9h18c0-2-3-2-3-9'></path><path d='M10 21h4'></path></svg><span id='notif-badge' class='app-badge hidden'>0</span></a>
          <details class='app-user'>
            <summary><span class='app-avatar'>%s</span><span class='app-uname'>u/%s</span></summary>
            <div class='app-user-menu'>
              <a href='/u/%s'>Profile</a>
              <a href='/settings'>Settings</a>
              <a href='/notifications'>Notifications</a>
              %s
              <form action='/logout' method='POST' class='app-logout'><button type='submit'>Log out</button></form>
            </div>
          </details>"
          initial u u admin_item
    | None ->
        "<a class='app-login' href='/login'>Log in</a>\
         <a class='app-cta' href='/signup'>Sign up</a>"
  in
  Printf.sprintf "
      <header class='app-topbar'>
        <div class='app-topbar-inner'>
          <a class='app-brand' href='/feed' aria-label='Earde feed'><img src='/static/images/logo-mark.svg' alt='' class='app-logo-mark'><img src='/static/images/logo-wordmark.svg' alt='Earde' class='app-logo-wordmark'></a>
          %s
          %s
          <div class='app-right'>%s</div>
        </div>
      </header>"
    nav search right

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

(* chrome selects the page furniture: `App = the mono command bar over a page that owns
   its own full-bleed layout (every shell/account/admin/creation surface plus the public
   read pages); `Auth = no chrome at all (the focused centered-card pages). The warm
   Tailwind `Site chrome — Nunito, the CDN runtime, navbar, footer — is fully retired. *)
let layout ?(noindex=false) ?user ?request ?(head_extra="") ?(chrome=`App) ?(gate=true) ~title content =
  let is_admin = match request with
    | Some req -> Dream.session_field req "is_admin" = Some "true"
    | None -> false
  in
  let robots_meta = if noindex then "<meta name='robots' content='noindex'>" else "" in

  (* `App surfaces carry the desktop-only gate: the stylesheet in <head> plus the panel in
     <body>. mobile-gate.css hides the app chrome and reveals the panel under the breakpoint;
     above it the panel stays display:none. ~gate:false lets PUBLIC read surfaces (feed,
     section feeds, threads, community home, search, privacy) opt out: gating archived
     knowledge off phones would break the product's "find it later" promise. *)
  let gate_css, gate_html = match chrome with
    | `App when gate -> mobile_gate_css_link, mobile_desktop_gate
    | `App | `Auth -> "", ""
  in

  let topbar_html = match chrome with
    | `App -> render_app_topbar ?user ?request ~is_admin ()
    (* `Auth: focused auth/account-lifecycle pages own the full viewport with their
       own centered card (auth.css) — no command bar. *)
    | `Auth -> ""
  in

  Printf.sprintf "
  <!DOCTYPE html>
  <html lang='en' class='scroll-smooth'>
  <head>
      <meta charset='UTF-8'>
      <meta name='viewport' content='width=device-width, initial-scale=1.0'>
      <title>%s - Earde</title>
      %s
      <link rel='preconnect' href='https://fonts.googleapis.com'>
      <link rel='preconnect' href='https://fonts.gstatic.com' crossorigin>
      <link href='https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600;700&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap' rel='stylesheet'>
      <link rel='stylesheet' href='/static/css/base.css'>
      %s
      %s
  </head>
  <body>
      <a class='skip-link' href='#main'>Skip to content</a>
      %s
      <main id='main'>
          %s
      </main>
      %s

      <script>
        /* Confirmation dialog: a native <dialog> + showModal(), which gives keyboard
           users focus trapping and Escape-to-cancel for free (the old hand-rolled
           overlay had neither). Styled by .mdlg in base.css — no framework classes. */
        function confirmModal(event, message) {
          event.preventDefault();
          const form = event.target;
          let dlg = document.getElementById('confirm-dialog');
          if (!dlg) {
            dlg = document.createElement('dialog');
            dlg.id = 'confirm-dialog';
            dlg.className = 'mdlg mdlg--confirm';
            dlg.innerHTML = `
              <div class='mdlg-head'><h3>Are you sure?</h3><p id='confirm-dialog-msg'></p></div>
              <div class='mdlg-actions'>
                <button type='button' class='mdlg-btn' data-act='cancel'>Cancel</button>
                <button type='button' class='mdlg-btn mdlg-confirm' data-act='confirm'>Confirm</button>
              </div>`;
            document.body.appendChild(dlg);
          }
          /* textContent prevents innerHTML XSS — message may contain user-supplied
             usernames (e.g., ban dialog). Using textContent treats the value as
             plain text regardless of what it contains. */
          dlg.querySelector('#confirm-dialog-msg').textContent = message;
          dlg.querySelector(\"[data-act='cancel']\").onclick = () => dlg.close();
          /* form.submit() bypasses the submit event so onsubmit won't re-fire. */
          dlg.querySelector(\"[data-act='confirm']\").onclick = () => { dlg.close(); form.submit(); };
          dlg.showModal();
        }
        /* Clipboard write is async; we optimistically swap innerHTML and class list
           rather than disabling the button — avoids layout shift on fast connections. */
        function copyPostLink(path, btn) {
          var fullUrl = window.location.origin + path;
          navigator.clipboard.writeText(fullUrl).then(function() {
            var originalHTML = btn.innerHTML;
            btn.innerHTML = '<svg class=\"w-4 h-4 mr-1 inline\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\"><path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M5 13l4 4L19 7\"></path></svg> Copied';
            btn.classList.add('text-emerald-600');
            btn.classList.remove('text-gray-500', 'hover:text-gray-900');
            setTimeout(function() {
              btn.innerHTML = originalHTML;
              btn.classList.remove('text-emerald-600');
              btn.classList.add('text-gray-500', 'hover:text-gray-900');
            }, 2000);
          }).catch(function(err) { console.error('Failed to copy: ', err); });
        }
        /* Optimistic vote update: mutate DOM immediately, then fire-and-forget XHR.
           If the request fails the server state is authoritative on next page load —
           acceptable UX trade-off for a forum where stale scores are low-stakes. */
        document.querySelectorAll(\"form[action='/vote'], form[action='/vote-comment']\").forEach(form => {
            form.addEventListener(\"submit\", async (e) => {
                e.preventDefault();
                const formData = new FormData(form);
                const urlEncodedData = new URLSearchParams(formData).toString();

                fetch(form.action, {
                    method: 'POST',
                    headers: {'Content-Type': 'application/x-www-form-urlencoded'},
                    body: urlEncodedData
                });

                const container = form.parentElement;
                const scoreSpan = container.querySelector(\"span\");
                let score = parseInt(scoreSpan.innerText);

                const upForm = container.firstElementChild;
                /* downForm may be absent when allow_downvotes=false — guard against
                   lastElementChild being the score <span> rather than a vote form. */
                const lastEl = container.lastElementChild;
                const downForm = (lastEl && lastEl.tagName === 'FORM') ? lastEl : null;

                const upBtn = upForm?.querySelector(\"button\");
                const downBtn = downForm?.querySelector(\"button\");

                const upInput = upForm?.querySelector(\"input[name='direction']\");
                const downInput = downForm?.querySelector(\"input[name='direction']\");

                const action = parseInt(formData.get(\"direction\"));
                const isUpvoteBtn = form === upForm;

                /* State colour is semantic: .is-active on .vote-up/.vote-down (base.css). */
                const setVote = (upActive, downActive) => {
                    upBtn?.classList.toggle(\"is-active\", upActive);
                    downBtn?.classList.toggle(\"is-active\", downActive);
                };

                if (action === 1) {
                    /* downInput===null means downvotes disabled → no prior downvote possible */
                    if (downInput && parseInt(downInput.value) === 0) score += 2;
                    else score += 1;
                    setVote(true, false);
                    if (upInput) upInput.value = \"0\";
                    if (downInput) downInput.value = \"-1\";
                }
                else if (action === -1) {
                    if (upInput && parseInt(upInput.value) === 0) score -= 2;
                    else score -= 1;
                    setVote(false, true);
                    if (upInput) upInput.value = \"1\";
                    if (downInput) downInput.value = \"0\";
                }
                else if (action === 0) {
                    if (isUpvoteBtn) score -= 1;
                    else score += 1;
                    setVote(false, false);
                    if (upInput) upInput.value = \"1\";
                    if (downInput) downInput.value = \"-1\";
                }

                scoreSpan.innerText = score;
            });
        });
      // Notif badge polling — fires once per page load to avoid repeated DB hits
        fetch('/api/unread-notifs')
            .then(response => response.text())
            .then(count => {
                let c = parseInt(count);
                if (c > 0) {
                    let badge = document.getElementById('notif-badge');
                    if (badge) {
                        badge.innerText = c;
                        badge.classList.remove('hidden');
                    }
                }
            }).catch(e => console.log(e));
      </script>
  </body>
  </html>"
  title robots_meta head_extra gate_css topbar_html content gate_html

(* Focused auth/account-lifecycle layout: a single centered card in the cool-grey
   shell idiom (auth.css), with no rail/sidebar/command bar. Uses `Auth chrome (no
   topbar, no footer) so .auth-shell can own the whole viewport. The
   brand mark mirrors render_app_topbar's and links to /feed. [card] is the inner
   card HTML the caller renders (form or message panel). *)
let auth_css_link = "<link rel='stylesheet' href='/static/css/auth.css'>"

let auth_page ?user ?(noindex=false) ?request ~title ~card () =
  let body =
    Printf.sprintf
      "<div class='auth-shell'>\
         <a class='auth-brand' href='/feed' aria-label='Earde feed'><img src='/static/images/logo-mark.svg' alt='' class='auth-logo-mark'><img src='/static/images/logo-wordmark.svg' alt='Earde' class='auth-logo-wordmark'></a>\
         <div class='auth-card'>%s</div>\
       </div>"
      card
  in
  layout ?user ?request ~noindex ~chrome:`Auth ~head_extra:auth_css_link ~title body

(* === HELPERS === *)

(* "[deleted_" is set by anonymize_user in Db — both sides must agree on the tombstone format. *)
let is_deleted_user u = String.length u >= 9 && String.sub u 0 9 = "[deleted_"

(* mod_usernames/admin_usernames enable badge rendering at call sites that know the community;
   callers without context omit the params, defaulting to [] so badge logic is a no-op. *)
let render_author ?(mod_usernames=[]) ?(admin_usernames=[]) username =
  if is_deleted_user username then
    "<span class='role-deleted'>[deleted]</span>"
  else
    let mod_badge =
      if List.mem username mod_usernames then " <span class='role mod'>MOD</span>"
      else ""
    in
    (* Admin badge is always site-wide; rendered after MOD so both appear side-by-side
       for the rare case where a site admin is also a local moderator. *)
    let admin_badge =
      if List.mem username admin_usernames then " <span class='role admin'>ADMIN</span>"
      else ""
    in
    Printf.sprintf "<a href='/u/%s' class='author-link'>u/%s</a>%s%s" (html_escape username) (html_escape username) mod_badge admin_badge

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

(* One renderer for every moderation reason-dialog (mod/admin remove + ban): a native
   <dialog> styled by .mdlg in base.css, opened with showModal() so focus trapping and
   Escape-to-cancel come from the platform. [variant] tints the heading by authority
   (`Mod = warning ochre ink, `Admin = signal red). [action] and [fields] are built by
   the caller (fields pre-escaped); the reason <textarea> is labelled via for/id. *)
let reason_dialog ~dialog_id ~variant ~heading ~note ~action ?maxlength ~placeholder ~confirm_label ~csrf_token ~fields () =
  let variant_cls = match variant with `Mod -> " mdlg--mod" | `Admin -> " mdlg--admin" in
  let maxlength_attr = match maxlength with
    | Some n -> Printf.sprintf " maxlength='%d'" n
    | None -> ""
  in
  Printf.sprintf
    "<dialog id='%s' class='mdlg%s'>\
       <div class='mdlg-head'><h3>%s</h3><p>%s</p></div>\
       <form action='%s' method='POST' class='mdlg-form'>\
         %s%s\
         <label class='mdlg-label' for='%s-reason'>Reason <span class='req'>*</span></label>\
         <textarea id='%s-reason' name='reason' required%s rows='4' placeholder='%s'></textarea>\
         <div class='mdlg-actions'>\
           <button type='button' class='mdlg-btn' onclick=\"document.getElementById('%s').close()\">Cancel</button>\
           <button type='submit' class='mdlg-btn mdlg-confirm'>%s</button>\
         </div>\
       </form>\
     </dialog>"
    dialog_id variant_cls heading note action csrf_token fields
    dialog_id dialog_id maxlength_attr placeholder dialog_id confirm_label

(* Session reads (current user, is_admin) happen inside a render function to avoid
   threading extra parameters through every call site — coupling is contained here. *)
(* is_current_user_mod and mod_usernames are optional — callers without community context
   (home feed, profile, search) omit them; they default to false/[] so delete
   visibility and author badges behave identically to before on those pages. *)
(* Shared post moderation/admin action markup: own-post Delete (Rule A), Mod/Admin Remove
   dialog (Rule B/C), and Mod/Admin Ban dialog. One renderer so the exact same forms, routes,
   CSRF tags, confirm/dialog behavior, required-reason fields and Rule A/B/C visibility are
   reused by every surface (e.g. search results, the legacy post-page fallback) with zero
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
  let remove_dialog variant =
    reason_dialog
      ~dialog_id:(Printf.sprintf "mod-modal-%d" post.id) ~variant
      ~heading:(match variant with `Mod -> "Moderator removal" | `Admin -> "Admin intervention")
      ~note:(match variant with
             | `Mod -> "This action is logged publicly in the mod log."
             | `Admin -> "This action is logged publicly as an admin override.")
      ~action:(Printf.sprintf "/c/%s/posts/%d/mod_delete" post.community_slug post.id)
      ~maxlength:255
      ~placeholder:(match variant with
                    | `Mod -> "Explain why this post is being removed (visible to the community)..."
                    | `Admin -> "Explain the admin intervention reason (visible to the community)...")
      ~confirm_label:"Confirm removal" ~csrf_token ~fields:"" ()
  in
  let action_btn =
    if is_already_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          (* Rule A: personal delete — no audit required *)
          Printf.sprintf "<form action='/delete-post' method='POST' class='act-form' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%d'>
              <button type='submit' class='act-btn act-btn--danger'>Delete</button>
          </form>" csrf_token post.id
        else if is_current_user_mod && not target_is_admin then
          (* Rule B: mod/top_mod removal — dialog enforces public reason *)
          Printf.sprintf
            "<button type='button' onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='act-btn act-btn--mod'>Mod remove</button>%s"
            post.id (remove_dialog `Mod)
        else if is_admin && not target_is_admin then
          (* Rule C: admin override (not a mod of this community) — logged as admin_delete_post *)
          Printf.sprintf
            "<button type='button' onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='act-btn act-btn--admin'>Admin remove</button>%s"
            post.id (remove_dialog `Admin)
        else ""
  in

  (* Ban button: mods exile per-community only; suppressed on home/profile feeds where
     is_current_user_mod defaults to false. Never shown for already-deleted accounts.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority
     so an admin who is also a mod doesn't bypass the public mod log via admin path. *)
  let ban_dialog variant =
    reason_dialog
      ~dialog_id:(Printf.sprintf "ban-modal-post-%d" post.id) ~variant
      ~heading:(match variant with `Mod -> "Mod ban" | `Admin -> "Admin ban")
      ~note:(match variant with
             | `Mod -> "This action is logged publicly in the mod log."
             | `Admin -> "This action is logged publicly as an admin override.")
      ~action:"/ban-community-user"
      ~placeholder:(match variant with
                    | `Mod -> "Explain why this user is being banned (visible to the community)..."
                    | `Admin -> "Explain the admin intervention reason (visible to the community)...")
      ~confirm_label:"Confirm ban" ~csrf_token
      ~fields:(Printf.sprintf
                 "<input type='hidden' name='target_username' value='%s'><input type='hidden' name='community_id' value='%d'>"
                 (html_escape post.username) post.community_id)
      ()
  in
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username && not (is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            "<span class='act-badge-banned'>Banned</span>"
          else if is_current_user_mod then
            (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
            Printf.sprintf
              "<button type='button' onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='act-btn act-btn--mod'>Mod ban</button>%s"
              post.id (ban_dialog `Mod)
          else
            (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
            Printf.sprintf
              "<button type='button' onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='act-btn act-btn--admin'>Admin ban</button>%s"
              post.id (ban_dialog `Admin)
      | _ -> ""
    else ""
  in
  action_btn ^ ban_btn

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

(* The feed row's ⋯ menu: the same moderation UI as post_admin_actions — identical routes,
   CSRF, dialog ids (via reason_dialog) and Rule A/B/C permission logic — packaged for the
   compact row context. Returns "" when the viewer has no available action (anon, or a
   tombstoned / admin-protected target) so the caller can omit the menu entirely. Dialog ids
   are keyed by post.id; the two renderers never appear on the same page. *)
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
     Rule B: mod/top_mod → Mod Remove dialog. Rule C: admin without mod role → Admin Remove.
     Dialogs come from the shared reason_dialog renderer (same ids as before). *)
  let remove_dialog variant =
    reason_dialog
      ~dialog_id:(Printf.sprintf "mod-modal-%d" post.id) ~variant
      ~heading:(match variant with `Mod -> "Moderator removal" | `Admin -> "Admin intervention")
      ~note:(match variant with
             | `Mod -> "This action is logged publicly in the mod log."
             | `Admin -> "This action is logged publicly as an admin override.")
      ~action:(Printf.sprintf "/c/%s/posts/%d/mod_delete" post.community_slug post.id)
      ~maxlength:255
      ~placeholder:(match variant with
                    | `Mod -> "Explain why this post is being removed (visible to the community)..."
                    | `Admin -> "Explain the admin intervention reason (visible to the community)...")
      ~confirm_label:"Confirm removal" ~csrf_token ~fields:"" ()
  in
  let action_btn =
    if is_already_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          Printf.sprintf "<form action='/delete-post' method='POST' class='act-form' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%d'>
              <button type='submit' class='act-btn act-btn--danger'>Delete</button>
          </form>" csrf_token post.id
        else if is_current_user_mod && not target_is_admin then
          Printf.sprintf
            "<button type='button' onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='act-btn act-btn--mod'>Mod remove</button>%s"
            post.id (remove_dialog `Mod)
        else if is_admin && not target_is_admin then
          Printf.sprintf
            "<button type='button' onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='act-btn act-btn--admin'>Admin remove</button>%s"
            post.id (remove_dialog `Admin)
        else ""
  in
  (* Ban: mods exile per-community; admins (without mod role) act via the override path. Mutually
     exclusive with the mod path so an admin who is also a mod still goes through the public mod
     log. A banned account shows a badge instead of a button to prevent double-ban confusion. *)
  let ban_dialog variant =
    reason_dialog
      ~dialog_id:(Printf.sprintf "ban-modal-post-%d" post.id) ~variant
      ~heading:(match variant with `Mod -> "Mod ban" | `Admin -> "Admin ban")
      ~note:(match variant with
             | `Mod -> "This action is logged publicly in the mod log."
             | `Admin -> "This action is logged publicly as an admin override.")
      ~action:"/ban-community-user"
      ~placeholder:(match variant with
                    | `Mod -> "Explain why this user is being banned (visible to the community)..."
                    | `Admin -> "Explain the admin intervention reason (visible to the community)...")
      ~confirm_label:"Confirm ban" ~csrf_token
      ~fields:(Printf.sprintf
                 "<input type='hidden' name='target_username' value='%s'><input type='hidden' name='community_id' value='%d'>"
                 (html_escape post.username) post.community_id)
      ()
  in
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username && not (is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            "<span class='act-badge-banned'>Banned</span>"
          else if is_current_user_mod then
            Printf.sprintf
              "<button type='button' onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='act-btn act-btn--mod'>Mod ban</button>%s"
              post.id (ban_dialog `Mod)
          else
            Printf.sprintf
              "<button type='button' onclick=\"document.getElementById('ban-modal-post-%d').showModal()\" class='act-btn act-btn--admin'>Admin ban</button>%s"
              post.id (ban_dialog `Admin)
      | _ -> ""
    else ""
  in
  action_btn ^ ban_btn

(* Thread-first feed row in the cool-grey shell idiom. Used by the section feed, the flat
   community feed, and the global feed. Vote markup follows the shared contract so the
   optimistic-vote JS (see layout) keeps working: the .cs-vote control's only direct children are
   the upvote form, the score <span>, and the downvote form, with the same Tailwind colour classes
   the JS toggles. The body is intentionally lighter than the card: strong title, an optional
   single-line preview, a compact monospace meta row, and moderation tucked into a ⋯ menu. *)
let render_forum_row ?(is_current_user_mod=false) ?(mod_usernames=[]) ?(admin_usernames=[]) ?(banned_usernames=[]) ?(show_context=false) request user_votes (post : post) =
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in
  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in
  (* Semantic vote classes (base.css); the optimistic-vote JS toggles .is-active. *)
  let up_color = if current_vote = 1 then "vote-up is-active" else "vote-up" in
  let down_color = if current_vote = -1 then "vote-down is-active" else "vote-down" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in
  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s' aria-label='Upvote'>▲</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='vote-up' aria-label='Log in to upvote'>▲</a>"
  in
  let downvote_html =
    if not post.allow_downvotes then ""
    else match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s' aria-label='Downvote'>▼</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='vote-down' aria-label='Log in to downvote'>▼</a>"
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
  (* ⋯ menu rendered only when the viewer actually has an action (keeps the feed clean, not admin-y). *)
  let mod_controls = mod_action_controls ~is_current_user_mod ~admin_usernames ~banned_usernames request post in
  let mod_menu =
    if mod_controls = "" then ""
    else Printf.sprintf "<details class='cs-row-mod'><summary>⋯</summary><div class='cs-row-mod-menu'>%s</div></details>" mod_controls
  in
  (* Internal links point at the canonical thread URL, not legacy /p/:id (which now 301s here). *)
  let thread_href = canonical_thread_path post.community_slug post.id post.title in
  (* show_context is set by the global Feed, where rows span communities/sections, so each row
     names its own origin: "§ Section · /c/slug". Section pages leave it off — the page header
     already supplies that context — so those views stay byte-for-byte unchanged. *)
  let context_html =
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

(* === COMMUNITY SHELL === *)

(* The persistent in-app community shell: a Discord-like multi-pane layout (community rail ·
   community sidebar · main pane · optional right pane) reused by future chat/forum/thread
   pages. This branch ships only the structural chrome — no chat, no realtime, no routes.

   nav_item/nav_group are shell-local with prefixed fields on purpose: Db.channel and
   Db.community_section both carry id/slug/name, so generic records here keep the shell
   decoupled from those schemas AND dodge record-field disambiguation. *)
type nav_item = {
  ni_label  : string;          (* display text; html-escaped here *)
  ni_href   : string;          (* target app path *)
  ni_sigil  : string;          (* "#" channel, "§" forum section, "" none *)
  ni_active : bool;            (* highlights the current page *)
  ni_badge  : string option;   (* optional right-aligned count/label *)
}

type nav_group = {
  ng_label : string;           (* group heading; "" renders an untitled group *)
  ng_items : nav_item list;
}

(* The shell's stylesheet is opt-in via layout's head_extra so it loads only on shell pages,
   keeping the "minimal CSS by default" rule. Scoped entirely under .community-shell. *)
let shell_css_link = "<link rel='stylesheet' href='/static/css/shell.css'>"

(* col 2 channel/section list. Each nav_item renders as a link so the shell works with no JS;
   active state is a class the CSS highlights. *)
let render_nav_item (it : nav_item) =
  let active = if it.ni_active then " active" else "" in
  let sigil =
    if it.ni_sigil = "" then ""
    else Printf.sprintf "<span class='cs-item-sigil'>%s</span>" (html_escape it.ni_sigil)
  in
  let badge = match it.ni_badge with
    | Some b -> Printf.sprintf "<span class='cs-item-badge'>%s</span>" (html_escape b)
    | None -> ""
  in
  (* Internal path, not an external URL — safe_internal_path keeps relative links navigable
     (safe_url would turn "/c/slug/ch/x" into "#"). *)
  Printf.sprintf "<a class='cs-item%s' href='%s'>%s<span class='cs-item-label'>%s</span>%s</a>"
    active (safe_internal_path it.ni_href) sigil (html_escape it.ni_label) badge

let render_nav_group (g : nav_group) =
  let label =
    if g.ng_label = "" then ""
    else Printf.sprintf "<div class='cs-group-label'>%s</div>" (html_escape g.ng_label)
  in
  let items = String.concat "\n" (List.map render_nav_item g.ng_items) in
  Printf.sprintf "<div class='cs-group'>%s%s</div>" label items

(* Which tile in the global rail is the current location. Drives the active marker without the
   rail needing to know about routing — feed_shell passes Rail_feed, community_shell passes
   Rail_community slug, and anything off the rail passes Rail_none. *)
type rail_active = Rail_feed | Rail_community of string | Rail_none

(* Tile glyph = first two letters of the slug (1 if short, "?" if empty). *)
let rail_glyph slug =
  if String.length slug >= 2 then String.sub slug 0 2
  else if String.length slug = 1 then slug
  else "?"

(* col 1 GLOBAL rail: the left icon rail is workspace-wide navigation, not community-only. It
   always renders Feed/home first, then one square per community the user belongs to, then the
   join/create + tile. Reuses Db.community (annotated like left_sidebar) — no new DB type, no
   Channel/Chat contact. One renderer for every shell surface so the tiles can never diverge. *)
let render_global_rail ~(active : rail_active) (communities : community list) =
  let feed_active = (match active with Rail_feed -> true | _ -> false) in
  let home =
    Printf.sprintf "<a class='cs-rail-item cs-rail-home%s' href='/feed' title='Feed'>⌂</a>"
      (if feed_active then " active" else "") in
  let tiles = List.map (fun (c : community) ->
    let is_active = (match active with Rail_community s -> s = c.slug | _ -> false) in
    let cls = if is_active then "cs-rail-item active" else "cs-rail-item" in
    (* Tile face: the community avatar when it's set and passes the image gate, else the existing
       2-letter slug glyph. NOT community_avatar — that falls back to a first-letter tile, but the
       rail's established empty-state is the 2-letter rail_glyph, kept here so avatarless tiles read
       exactly as before. The <img> fills the fixed 42x42 tile (see shell.css), so there's no
       layout shift between the image and glyph cases. *)
    let face =
      match c.avatar_url with
      | Some url when String.trim url <> "" ->
          let src = safe_img_src url in
          if src = "#" then html_escape (rail_glyph c.slug)
          else Printf.sprintf "<img class='cs-rail-img' src='%s' alt='' loading='lazy' decoding='async'>" src
      | _ -> html_escape (rail_glyph c.slug)
    in
    (* The rail is a workspace switcher: a tile means "enter this community", so it targets the
       default chat channel (every community has a "general" channel after the default-structure
       merge), NOT the legacy /c/:slug overview, which isn't a shell page yet. A future branch can
       swap this for a shell overview or last-visited destination. *)
    Printf.sprintf "<a class='%s' href='/c/%s/ch/general' title='/c/%s'>%s</a>"
      cls (html_escape c.slug) (html_escape c.slug) face
  ) communities in
  let add =
    "<a class='cs-rail-item cs-rail-add' href='/new-community' title='Join or start a community'>+</a>" in
  Printf.sprintf "<nav class='cs-rail'>%s%s%s</nav>"
    home (String.concat "\n" tiles) add

(* col 2 community sidebar: header (small avatar + community name + /c/slug) then the nav groups.
   The avatar reinforces "where am I"; community_avatar falls back to a name letter-tile (never a
   broken image) so avatarless communities keep a clean header. *)
let render_sidebar (community : community) (nav_groups : nav_group list) =
  let groups = String.concat "\n" (List.map render_nav_group nav_groups) in
  let avatar =
    community_avatar ~img_class:"cs-side-avatar"
      ~tile_class:"cs-side-avatar cs-side-avatar--mono" ~name:community.name community.avatar_url
  in
  let community_href = Printf.sprintf "/c/%s" community.slug in
  Printf.sprintf "
    <aside class='cs-side'>
      <div class='cs-side-head'>
        <a class='cs-side-head-link' href='%s'>
          %s
          <span class='cs-side-head-text'>
            <span class='cs-side-name'>%s</span>
            <span class='cs-side-slug'>/c/%s</span>
          </span>
        </a>
      </div>
      <div class='cs-side-scroll'>%s</div>
    </aside>"
    (safe_internal_path community_href)
    avatar (html_escape community.name) (html_escape community.slug) groups

(* === GLOBAL APP SHELL ===
   The base shell for every in-app surface (feed, channel, section, thread). It owns the layout
   wrapper (shell stylesheet), the global icon rail, the main pane, an OPTIONAL
   local sidebar (community channels/sections), and an OPTIONAL right pane. The shell is NOT
   community-only: Feed is a shell surface too. community_shell / feed_shell are thin wrappers
   that differ only in the local sidebar and which rail tile is active.
   `main`/`sidebar`/`right_pane` are caller-rendered HTML fragments. *)
let global_shell ?user ?request ?noindex ?(rail_communities=[]) ~(rail_active : rail_active)
    ?sidebar ?right_pane ?(head_extra="") ?(gate=true) ~title ~main () =
  let rail = render_global_rail ~active:rail_active rail_communities in
  let sidebar_html = Option.value sidebar ~default:"" in
  let aside = match right_pane with
    | Some html -> Printf.sprintf "<aside class='cs-aside'>%s</aside>" html
    | None -> ""
  in
  (* .community-shell keeps the existing scoped CSS working unchanged; .app-shell is a semantic
     alias so future code stops treating the whole shell as community-only. Without a local
     sidebar we add .feed-shell, whose grid drops the col-2 column. with-aside switches to the
     wider grid; the CSS hides the aside on narrow viewports. *)
  let base = "community-shell app-shell" in
  let base = if sidebar = None then base ^ " feed-shell" else base in
  let shell_cls = if right_pane = None then base else base ^ " with-aside" in
  (* The centre pane is a <div>: layout already wraps the whole grid in the page's single
     <main> landmark, and nested <main> elements are invalid HTML that confuses AT. *)
  let grid =
    Printf.sprintf "<div class='%s'>%s%s<div class='cs-main'>%s</div>%s</div>"
      shell_cls rail sidebar_html main aside
  in
  (* head_extra lets a shell page add per-page <head> tags (canonical, meta description, a
     page-scoped script) after the shell stylesheet; default "" keeps pages byte-for-byte
     unchanged. *)
  layout ?noindex ?user ?request ~head_extra:(shell_css_link ^ head_extra) ~chrome:`App ~gate ~title grid

(* Phone-width community navigation: the col-2 sidebar disappears under 820px, so
   un-gated read surfaces (section feeds, threads) get an SSR <details> disclosure at
   the top of the main pane instead — same nav_groups markup, so channels/sections
   stay one render path. Pure CSS reveal (.cs-mobile-nav, hidden ≥821px); no JS. *)
let render_mobile_community_nav (community : community) (nav_groups : nav_group list) =
  let groups = String.concat "\n" (List.map render_nav_group nav_groups) in
  Printf.sprintf
    "<details class='cs-mobile-nav'>\
       <summary>\
         <span class='cs-mobile-nav-name'>%s</span>\
         <span class='cs-mobile-nav-slug'>/c/%s</span>\
         <span class='cs-mobile-nav-caret' aria-hidden='true'>&#9662;</span>\
       </summary>\
       <div class='cs-mobile-nav-panel'>%s</div>\
     </details>"
    (html_escape community.name) (html_escape community.slug) groups

(* The community shell: global app shell + the local community sidebar (channels + forum
   sections). active_slug lights up this community's rail tile. Public signature unchanged so
   existing callers (section/channel/thread pages) need no edits. *)
let community_shell ?user ?request ?noindex ?(rail_communities=[]) ?active_slug
    ?right_pane ?(head_extra="") ?(gate=true) ~title ~community ~nav_groups ~main () =
  let sidebar = render_sidebar community nav_groups in
  let rail_active = match active_slug with Some s -> Rail_community s | None -> Rail_none in
  (* The mobile nav rides inside the main pane (first child) so it never disturbs the
     desktop grid; CSS reveals it only below the sidebar breakpoint. Gated surfaces
     (chat) render it too, but the gate hides the whole app there before it matters. *)
  let main = render_mobile_community_nav community nav_groups ^ main in
  global_shell ?user ?request ?noindex ~rail_communities ~rail_active
    ~sidebar ?right_pane ~head_extra ~gate ~title ~main ()

(* The global Feed shell: the same app shell with NO community sidebar (Feed lives outside any
   one community) and the Feed rail tile active. Feed is a public read surface, so it opts
   out of the desktop-only gate (shell.css collapses it to one column under 640px). *)
let feed_shell ?user ?request ?noindex ?(rail_communities=[]) ?right_pane ?(head_extra="") ~title ~main () =
  global_shell ?user ?request ?noindex ~rail_communities ~rail_active:Rail_feed
    ?right_pane ~head_extra ~gate:false ~title ~main ()

(* Focused in-product creation layout: the mono app command bar (so the page stays in
   the app, coherent with the topbar's "+ Start community" CTA) over a single centered
   cool-grey panel on a graph-paper background (create.css), with no rail/sidebar — long
   forms read better in one column than wedged into the multi-pane shell grid. shell.css
   is loaded for the .app-topbar styles; create.css owns everything under .create-shell.
   [body] is the inner page HTML the caller renders (the form, list, or gate panel). *)
let create_css_link = "<link rel='stylesheet' href='/static/css/create.css'>"

let create_page ?user ?request ?(noindex=false) ~title ~body () =
  let shell =
    Printf.sprintf "<div class='create-shell'>%s</div>" body
  in
  layout ?user ?request ~noindex ~chrome:`App
    ~head_extra:(shell_css_link ^ create_css_link) ~title shell

(* Focused in-product account layout: the mono app command bar over a single centered
   cool-grey column (account.css), with no rail/sidebar/footer — the personal account
   pages (profile / settings / notifications) are reading/editing surfaces, not the
   multi-pane community workspace, so they read better in one focused column. shell.css
   is loaded for the .app-topbar styles AND the .cs-thread/.ft-* row styles that the
   profile's post list reuses via render_forum_row; account.css owns everything under
   .account-shell. [body] is the inner page HTML the caller renders. *)
let account_css_link = "<link rel='stylesheet' href='/static/css/account.css'>"

let account_page ?user ?request ?(noindex=false) ?(gate=true) ~title ~body () =
  let shell =
    Printf.sprintf "<div class='account-shell'>%s</div>" body
  in
  layout ?user ?request ~noindex ~chrome:`App ~gate
    ~head_extra:(shell_css_link ^ account_css_link) ~title shell

(* Focused in-product admin layout: the mono app command bar over a single centered
   cool-grey column (admin.css), with no rail/sidebar/footer. Same idiom as account_page;
   used only by the admin-only /admin dashboard. shell.css is loaded for .app-topbar;
   admin.css owns everything under .admin-shell. The handler always gates this on the
   is_admin session field — this wrapper adds NO authorization of its own. *)
let admin_css_link = "<link rel='stylesheet' href='/static/css/admin.css'>"

let admin_page ?user ?request ?(noindex=false) ~title ~body () =
  let shell =
    Printf.sprintf "<div class='admin-shell'>%s</div>" body
  in
  layout ?user ?request ~noindex ~chrome:`App
    ~head_extra:(shell_css_link ^ admin_css_link) ~title shell

(* Focused in-product community-management layout: the mono app command bar over
   a single centered cool-grey column (community-manage.css), with no
   rail/sidebar/footer. Same idiom as admin_page; shared by the per-community
   management surfaces (/c/:slug/settings, /c/:slug/manage-mods, /c/:slug/modlog)
   so they read as one operator console. shell.css is loaded for .app-topbar;
   community-manage.css owns everything under .cm-shell. The handlers gate these
   on mod/top_mod/admin authority — this wrapper adds NO authorization of its own
   (and modlog stays public exactly as the handler allows). *)
let community_manage_css_link = "<link rel='stylesheet' href='/static/css/community-manage.css'>"

let community_manage_page ?user ?request ?(noindex=false) ~title ~body () =
  let shell =
    Printf.sprintf "<div class='cm-shell'>%s</div>" body
  in
  layout ?user ?request ~noindex ~chrome:`App
    ~head_extra:(shell_css_link ^ community_manage_css_link) ~title shell

(* The public community home (/c/:slug). Unlike the focused single-column wrappers above it
   does NOT wrap [body] in an extra shell div — the page already emits its own full-bleed
   .community-home structure (hero band + checkered body). It only supplies the App command bar
   (no warm navbar, no footer) and loads shell.css (for .app-topbar) + community-home.css (which
   owns everything under .community-home). SSR-only: every link/form works with JS off. *)
let community_home_css_link = "<link rel='stylesheet' href='/static/css/community-home.css'>"

let community_home_page ?user ?request ?(noindex=false) ~title ~body () =
  (* Public archive surface: no desktop-only gate — the page is a single scrolling
     column and already responsive at phone widths. *)
  layout ?user ?request ~noindex ~chrome:`App ~gate:false
    ~head_extra:(shell_css_link ^ community_home_css_link) ~title body

(* Focused in-product search layout: the mono app command bar over a single centered
   cool-grey column (search.css), with no rail/sidebar/footer — search is a reading/retrieval
   surface, not the multi-pane community workspace. Same idiom as account_page. shell.css is
   loaded for .app-topbar; search.css owns everything under .search-shell. [body] is the
   inner page HTML the caller renders (header + tabs + result rows). SSR-only. *)
let search_css_link = "<link rel='stylesheet' href='/static/css/search.css'>"

let search_page ?user ?request ?(noindex=false) ~title ~body () =
  let shell =
    Printf.sprintf "<div class='search-shell'>%s</div>" body
  in
  (* Knowledge retrieval is the product promise: search stays usable on phones
     (single centered column), so it opts out of the desktop-only gate. *)
  layout ?user ?request ~noindex ~chrome:`App ~gate:false
    ~head_extra:(shell_css_link ^ search_css_link) ~title shell
