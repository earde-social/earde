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

(* === LAYOUT === *)

(* CDN Tailwind avoids a build step — acceptable for dev/staging; swap for a
   bundled stylesheet before serving under high traffic to remove the round-trip. *)
(* request is optional so callers without session context (e.g. choose_community_page)
   can omit it and get is_admin=false by default — no forced parameter threading. *)
let layout ?(noindex=false) ?user ?request ~title content =
  let is_admin = match request with
    | Some req -> Dream.session_field req "is_admin" = Some "true"
    | None -> false
  in
  let auth_menu =
    match user with
    | Some username ->
        (* Admin link: shown only in-session to keep the navbar uncluttered for
           non-admins; linking to /admin exposes no data on its own — the handler
           re-checks is_admin before rendering anything sensitive. *)
        let admin_link =
          if is_admin then
            "<a href='/admin' class='text-red-600 hover:text-red-800 font-semibold text-xs border border-red-200 px-2 py-1 rounded-md hover:bg-red-50 transition' title='Admin Dashboard'>🛡️ Admin</a>"
          else ""
        in
        Printf.sprintf "
          <div class='flex flex-row items-center gap-2 sm:gap-3'>
            <a href='/u/%s' class='hidden sm:block text-gray-600 hover:text-[#0D9488] font-medium text-sm transition'>u/%s</a>
            %s
            <a href='/notifications' class='relative text-gray-400 hover:text-gray-600 text-base' title='Notifications'>
                🔔 <span id='notif-badge' class='hidden absolute -top-1 -right-2 bg-red-500 text-white text-[10px] font-bold px-1.5 py-0.5 rounded-full'>0</span>
            </a>
            <form action='/logout' method='POST' class='m-0 p-0 flex items-center'>
               <button type='submit' class='text-xs text-gray-500 hover:text-red-600 font-medium border border-gray-200 px-3 py-1 rounded-md hover:border-red-200 transition'>
                 Log out
               </button>
            </form>
          </div>"
        (html_escape username) (html_escape username) admin_link
    | None ->
        "<div class='flex items-center space-x-3'>
           <a href='/login' class='text-gray-600 hover:text-[#0D9488] font-medium text-sm transition'>Log in</a>
           <a href='/signup' class='bg-[#0D9488] text-white px-4 py-1.5 rounded-md hover:bg-teal-700 font-semibold text-sm transition'>Sign up</a>
         </div>"
  in

  let robots_meta = if noindex then "<meta name='robots' content='noindex'>" else "" in

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
      <link href='https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap' rel='stylesheet'>
      <script src='https://cdn.tailwindcss.com'></script>
      <style>body { font-family: 'Inter', sans-serif; }</style>
  </head>
  <body class='bg-[#FDFCFB] text-gray-900 min-h-screen flex flex-col'>
      <nav class='bg-white border-b border-gray-200 sticky top-0 z-50'>
          <div class='w-full px-4 md:px-6 h-14 flex items-center justify-between'>
              <div class='flex-shrink-0 w-48 lg:w-56 flex items-center'>
                  <a href='/' class='text-xl font-bold text-[#0D9488] tracking-tight'>Earde</a>
              </div>
              <div class='flex-1 flex justify-center max-w-2xl px-4'>
                  <form action='/search' method='GET' class='hidden md:flex items-center w-full'>
                      <div class='relative w-full'>
                          <div class='absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none'>
                              <span class='text-gray-400 text-sm'>🔍</span>
                          </div>
                          <input type='text' name='q' placeholder='Search communities, users, posts...' required
                                 class='w-full pl-9 pr-4 py-1.5 bg-gray-100 border border-transparent text-gray-900 text-sm rounded-md focus:bg-white focus:border-[#0D9488] focus:ring-1 focus:outline-none transition'>
                      </div>
                  </form>
              </div>
              <div class='flex flex-row items-center justify-end gap-2 sm:gap-4 shrink-0'>
                  %s
              </div>
          </div>
      </nav>
      <main class='flex-grow max-w-[1600px] w-full mx-auto px-6 sm:px-8 py-6'>
          %s
      </main>
      <footer class='border-t border-gray-100 mt-auto'>
          <div class='max-w-screen-2xl mx-auto py-4 px-6 sm:px-8'>
              <p class='text-center text-gray-400 text-xs'>&copy; 2026 Earde &middot; <a href='/about' class='hover:text-[#0D9488] transition'>About</a> &middot; <a href='/privacy' class='hover:text-[#0D9488] transition'>Privacy</a></p>
          </div>
      </footer>

      <script>
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
              <button type='button' class='px-4 py-2 text-sm font-medium text-gray-700 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-[#0D9488]' id='cancel-btn'>Cancel</button>
              <button type='button' class='px-4 py-2 text-sm font-medium text-white bg-red-600 border border-transparent rounded-lg hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-600' id='confirm-btn'>Confirm</button>
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
                const downForm = container.lastElementChild;

                const upBtn = upForm.querySelector(\"button\");
                const downBtn = downForm.querySelector(\"button\");

                const upInput = upForm.querySelector(\"input[name='direction']\");
                const downInput = downForm.querySelector(\"input[name='direction']\");

                const action = parseInt(formData.get(\"direction\"));
                const isUpvoteBtn = form === upForm;

                const resetColors = () => {
                    upBtn.classList.remove(\"text-orange-500\");
                    upBtn.classList.add(\"text-gray-400\", \"hover:text-orange-500\");
                    downBtn.classList.remove(\"text-[#0D9488]\");
                    downBtn.classList.add(\"text-gray-400\", \"hover:text-[#0D9488]\");
                };

                if (action === 1) {
                    if (parseInt(downInput.value) === 0) score += 2;
                    else score += 1;
                    resetColors();
                    upBtn.classList.remove(\"text-gray-400\", \"hover:text-orange-500\");
                    upBtn.classList.add(\"text-orange-500\");
                    upInput.value = \"0\";
                    downInput.value = \"-1\";
                }
                else if (action === -1) {
                    if (parseInt(upInput.value) === 0) score -= 2;
                    else score -= 1;
                    resetColors();
                    downBtn.classList.remove(\"text-gray-400\", \"hover:text-[#0D9488]\");
                    downBtn.classList.add(\"text-[#0D9488]\");
                    upInput.value = \"1\";
                    downInput.value = \"0\";
                }
                else if (action === 0) {
                    if (isUpvoteBtn) score -= 1;
                    else score += 1;
                    resetColors();
                    upInput.value = \"1\";
                    downInput.value = \"-1\";
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
  title robots_meta auth_menu content

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
    Printf.sprintf "<a href='/u/%s' class='hover:text-[#0D9488] hover:underline font-medium transition'>u/%s</a>%s%s" (html_escape username) (html_escape username) mod_badge admin_badge

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
    let epoch, _ = Unix.mktime tm in
    let now = Unix.time () in
    let diff = int_of_float (now -. epoch) in

    if diff < 0 then "just now"
    else if diff < 60 then "just now"
    else if diff < 3600 then Printf.sprintf "%d min ago" (diff / 60)
    else if diff < 86400 then Printf.sprintf "%d hr ago" (diff / 3600)
    else if diff < 2592000 then Printf.sprintf "%d days ago" (diff / 86400)
    else if diff < 31536000 then Printf.sprintf "%d mo ago" (diff / 2592000)
    else Printf.sprintf "%d yr ago" (diff / 31536000)
  with _ ->
    date_str

(* === CARDS === *)

(* Session reads (current user, is_admin) happen inside a render function to avoid
   threading extra parameters through every call site — coupling is contained here. *)
(* is_current_user_mod and mod_usernames are optional — callers without community context
   (home feed, profile, search) omit them; they default to false/[] so delete
   visibility and author badges behave identically to before on those pages. *)
let render_post ?(is_current_user_mod=false) ?(mod_usernames=[]) ?(admin_usernames=[]) ?(banned_usernames=[]) request user_votes (post : post) =
  let csrf_token = Dream.csrf_tag request in
  let content_preview = Option.value ~default:"" post.content in
  let link_part = match post.url with | Some u -> Printf.sprintf "<a href='%s' class='text-xs text-[#0D9488] hover:underline' target='_blank'>%s ↗</a>" (safe_url u) (html_escape u) | None -> "" in

  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in

  let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote = -1 then "text-[#0D9488]" else "text-gray-400 hover:text-[#0D9488]" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in

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
  let delete_btn =
    if is_already_deleted then ""
    else match current_user with
    | Some u when u = post.username || is_admin || (is_current_user_mod && not target_is_admin) ->
        Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0 ml-2' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
            %s <input type='hidden' name='post_id' value='%d'>
            <button type='submit' class='text-xs %s hover:text-red-700 opacity-50 hover:opacity-100 transition'>🗑️</button>
        </form>" csrf_token post.id (if u <> post.username then "text-red-500" else "text-gray-400")
    | _ -> ""
  in

  (* Ban button: mods exile per-community only; suppressed on home/profile feeds where
     is_current_user_mod defaults to false. Never shown for already-deleted accounts.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Admins share mod authority site-wide, so the same button is granted to them. *)
  let ban_btn =
    if (is_current_user_mod || is_admin) && not (target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username && not (is_deleted_user post.username) ->
          if List.mem post.username banned_usernames then
            "<span class='text-xs text-red-500 font-semibold ml-2'>🚫 Banned</span>"
          else
            Printf.sprintf "<form action='/ban-community-user' method='POST' class='inline m-0 p-0 ml-1' onsubmit=\"confirmModal(event, 'Ban this user from this community? This action cannot be undone.')\">%s<input type='hidden' name='target_username' value='%s'><input type='hidden' name='community_id' value='%d'><button type='submit' class='text-xs text-gray-400 hover:text-orange-500 transition opacity-50 hover:opacity-100' title='Ban user from this community'>🔨</button></form>"
              csrf_token (html_escape post.username) post.community_id
      | _ -> ""
    else ""
  in

  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▲</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 font-bold text-sm leading-none'>▲</a>"
  in
  let downvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▼</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-[#0D9488] font-bold text-sm leading-none'>▼</a>"
  in

  Printf.sprintf "
  <div onclick=\"if(!event.target.closest('a, button, form')) window.location='/p/%d'\" class='cursor-pointer border-b border-gray-100 py-4 flex gap-4 hover:bg-gray-50/50 transition-colors'>

      <div class='flex flex-col items-center pt-0.5 w-7 shrink-0 cursor-default' onclick=\"event.stopPropagation()\">
          %s
          <span class='font-semibold text-gray-600 text-xs my-0.5'>%d</span>
          %s
      </div>

      <div class='flex-1 min-w-0'>
          <div class='flex flex-wrap items-center gap-x-1.5 text-xs text-gray-500 mb-1'>
              <a href='/c/%s' class='font-semibold text-gray-700 hover:text-[#0D9488] transition relative z-10'>/c/%s</a>
              <span class='text-gray-300'>•</span>
              <span>by</span>
              <span class='relative z-10'>%s</span>
              <span class='text-gray-300'>•</span>
              <span>%s</span>
              <span class='relative z-10'>%s</span>
          </div>

          <h3 class='text-base font-semibold text-gray-900 leading-snug mb-1'>
              <a href='/p/%d' class='hover:text-[#0D9488] transition break-words'>%s</a>
          </h3>

          <div class='relative z-10 text-xs'>%s</div>
          <p class='text-sm text-gray-600 mt-2 break-words line-clamp-6'>%s</p>

          <div class='flex items-center mt-2 text-xs text-gray-400'>
              <a href='/p/%d' class='hover:text-[#0D9488] flex items-center gap-1 transition relative z-10'>
                  <span>💬</span><span>%d comments</span>
              </a>
              <button type='button' onclick='copyPostLink(\"/p/%d\", this)' class='text-xs font-medium text-gray-500 hover:text-gray-900 flex items-center transition-colors cursor-pointer ml-4'>🔗 Share</button>
          </div>
      </div>
  </div>"
  post.id
  upvote_html post.score downvote_html
  (html_escape post.community_slug) (html_escape post.community_slug) (render_author ~mod_usernames ~admin_usernames post.username) (time_ago post.created_at) (delete_btn ^ ban_btn)
  post.id (html_escape post.title) link_part (html_escape content_preview) post.id post.comment_count post.id

let community_card (community : community) =
  Printf.sprintf "
  <div class='bg-white rounded-lg p-5 border border-gray-200 hover:border-[#0D9488] transition border-l-4 border-l-[#0D9488]'>
      <h2 class='text-base font-semibold text-gray-900 mb-1'>%s</h2>
      <div class='text-xs text-[#0D9488] mb-2 font-mono'>/c/%s</div>
      <p class='text-gray-500 text-sm'>%s</p>
  </div>"
    (html_escape community.name)
    (html_escape community.slug)
    (html_escape (Option.value ~default:"No description." community.description))

(* Shared left nav: drives the "Your Communities" list on index, community, and post pages.
   Extracted to avoid divergent copies of the same community list markup. *)
let left_sidebar ?user ~moderated_communities (user_communities : community list) =
  match user with
  | None ->
      "<div class='bg-teal-50 p-4 rounded-xl border border-teal-100'><h3 class='font-semibold text-gray-900 mb-1 text-sm'>Join Earde</h3><p class='text-xs text-gray-600 mb-3'>Create an account to follow communities and join the conversation.</p><a href='/signup' class='block w-full bg-[#0D9488] text-white text-center py-2 rounded-md font-semibold text-sm hover:bg-teal-700 transition'>Sign Up</a></div>"
  | Some _ ->
      if user_communities = [] && moderated_communities = [] then
        "<div class='p-4 bg-white rounded-xl border border-gray-200 shadow-sm'><p class='text-sm text-gray-500 mb-3'>You haven't joined any communities yet.</p><a href='/new-community' class='text-[#0D9488] font-bold text-sm hover:underline'>Create one &rarr;</a></div>"
      else
        (* Dedup: following list excludes communities the user already moderates. *)
        let mod_ids = List.map (fun (a : community) -> a.id) moderated_communities in
        let following_communities = List.filter (fun (a : community) -> not (List.mem a.id mod_ids)) user_communities in
        let home_link = "<li><a href='/' class='flex items-center space-x-2 p-2 rounded-md hover:bg-gray-50 text-gray-700 font-medium transition'><span class='text-gray-400 mr-1'>🏠</span><span class='truncate'>Home</span></a></li>" in
        let mod_section =
          if moderated_communities = [] then ""
          else
            let mod_items = List.map (fun (a : community) ->
              Printf.sprintf "<li><a href='/c/%s' class='flex items-center space-x-2 p-2 rounded-md hover:bg-gray-50 text-gray-700 font-medium transition'><span class='text-teal-400 font-bold'>/c/</span><span class='truncate'>%s</span><span class='ml-auto text-xs' title='Moderating'>🛡️</span></a></li>"
                (html_escape a.slug) (html_escape a.name)
            ) moderated_communities in
            Printf.sprintf "<h3 class='px-2 mb-1 text-xs font-semibold text-gray-500 uppercase tracking-wider mt-3'>Moderating</h3><ul class='space-y-1'>%s</ul>" (String.concat "\n" mod_items)
        in
        let follow_section =
          if following_communities = [] then ""
          else
            let items = List.map (fun (a : community) ->
              Printf.sprintf "<li><a href='/c/%s' class='flex items-center space-x-2 p-2 rounded-md hover:bg-gray-50 text-gray-700 font-medium transition'><span class='text-teal-400 font-bold'>/c/</span><span class='truncate'>%s</span></a></li>"
                (html_escape a.slug) (html_escape a.name)
            ) following_communities in
            Printf.sprintf "<h3 class='px-2 mb-1 text-xs font-semibold text-gray-500 uppercase tracking-wider mt-3'>Following</h3><ul class='space-y-1'>%s</ul>" (String.concat "\n" items)
        in
        Printf.sprintf "<div class='bg-white p-4 rounded-xl border border-gray-200 shadow-sm'><ul class='space-y-1'>%s</ul>%s%s</div>"
          home_link mod_section follow_section
