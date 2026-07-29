(* Bound before `open Db`, which would otherwise shadow the top-level
   Analytics module with Db.Analytics. *)
module Posthog = Analytics

open Db

(* === CORE FEED === *)

let index ?user user_votes current_page sort_mode ~feed_type ~admin_usernames ~moderated_communities (posts : post list) (user_communities : community list) request =
  let has_next = List.length posts = 20 in
  (* base_url drives pagination and sort links so they stay within the correct feed *)
  let base_url = if feed_type = "home" then "/" else "/all" in

  let posts_html =
    if posts = [] then
      "<div class='text-center py-10 text-gray-500 border border-dashed border-[#E0D9CC] rounded-xl'>It's quiet here. Too quiet. <br><a href='/bring' class='text-[#C94C4C] underline'>Bring your community</a> and start posting!</div>"
    else String.concat "\n" (List.map (Components.render_post ~admin_usernames request user_votes) posts)
  in

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='%s?sort=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-[#EDE9DF] transition'>&larr; Prev</a>" base_url sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='%s?sort=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-[#EDE9DF] transition'>Next &rarr;</a>" base_url sort_mode (current_page + 1) in

  let get_sort_class s = if s = sort_mode then "text-[#C94C4C] border-b-2 border-[#C94C4C] pb-1" else "text-gray-500 hover:text-gray-800 transition" in
  let sort_menu = Printf.sprintf "
    <div class='flex space-x-6 mb-6 px-2 border-b border-[#E0D9CC]'>
        <a href='%s?sort=hot' class='font-bold text-sm tracking-wide uppercase %s'>🔥 Hot</a>
        <a href='%s?sort=new' class='font-bold text-sm tracking-wide uppercase %s'>✨ New</a>
        <a href='%s?sort=top' class='font-bold text-sm tracking-wide uppercase %s'>🏆 Top</a>
    </div>" base_url (get_sort_class "hot") base_url (get_sort_class "new") base_url (get_sort_class "top")
  in

  (* Feed toggle: active tab gets a teal bottom border; inactive is muted *)
  let get_tab_class t = if t = feed_type then "font-bold text-[#C94C4C] border-b-2 border-[#C94C4C] pb-2" else "font-medium text-gray-500 hover:text-gray-800 pb-2 transition" in
  let feed_tabs = Printf.sprintf "
    <div class='flex space-x-6 mb-4 border-b border-gray-100'>
        <a href='/' class='%s'>Home</a>
        <a href='/all' class='%s'>All</a>
    </div>" (get_tab_class "home") (get_tab_class "all")
  in

  let feed_title = if feed_type = "home" then "Home" else "All" in

  let sidebar_html = Components.left_sidebar ?user ~moderated_communities user_communities in

  let content = Printf.sprintf "
    <div class='flex flex-col lg:flex-row gap-6'>
        <div class='w-full lg:w-1/4 hidden lg:block'><div class='sticky top-20'>%s</div></div>
        <div class='w-full lg:w-2/4 min-w-0'>
            <div class='flex justify-between items-center mb-4'>
                <h1 class='text-2xl font-bold text-gray-900'>%s</h1>
            </div>
            %s
            <div class='block lg:hidden mb-6 bg-blue-50 border border-blue-100 rounded-xl p-4 shadow-sm'><h3 class='text-sm font-bold text-blue-900 mb-1'>Talk to me!</h3><p class='text-xs text-blue-800 mb-3 leading-relaxed'>For feature requests, ideas, critiques, if you are a Reddit mod and want to become a mod on the specular community here, or just to say hi!</p><a href='https://t.me/tolwiz' target='_blank' rel='noopener noreferrer' class='w-full flex items-center justify-center gap-2 bg-blue-600 hover:bg-blue-700 text-white text-sm font-semibold py-2 rounded-xl transition-colors'>&#128172; Text me (the dev)!</a></div>
            %s <div>%s</div>
            <div class='flex justify-between items-center mt-8 mb-4'>
                <div>%s</div><div class='text-sm text-gray-500 font-bold'>Page %d</div><div>%s</div>
            </div>
        </div>
        <div class='w-full lg:w-1/4'><div class='bg-white p-5 rounded-xl border border-[#E0D9CC] sticky top-20'><h2 class='text-sm font-semibold text-gray-800 mb-1'>Earde</h2><p class='text-xs text-gray-500 mb-4'>Your personal frontpage.</p><div class='flex flex-col space-y-2'><a href='/new-post' class='w-full bg-[#C94C4C] text-white text-center py-2 rounded-xl font-semibold text-sm hover:bg-[#A83A3A] transition'>Create Post</a><a href='/bring' class='w-full bg-white text-[#C94C4C] border border-[#C94C4C] text-center py-2 rounded-xl font-semibold text-sm hover:bg-[#F0EDE4] transition'>Connect a project</a></div></div><div class='mt-6 bg-blue-50 border border-blue-100 rounded-xl p-4 shadow-sm'><h3 class='text-sm font-bold text-blue-900 mb-1'>Talk to me!</h3><p class='text-xs text-blue-800 mb-3 leading-relaxed'>For feature requests, ideas, critiques, if you are a Reddit mod and want to become a mod on the specular community here, or just to say hi!</p><a href='https://t.me/tolwiz' target='_blank' rel='noopener noreferrer' class='w-full flex items-center justify-center gap-2 bg-blue-600 hover:bg-blue-700 text-white text-sm font-semibold py-2 rounded-xl transition-colors'>&#128172; Text me (the dev)!</a></div></div>
    </div>"
    sidebar_html feed_title feed_tabs sort_menu posts_html prev_btn current_page next_btn
  in
  Components.layout ?user ~request ~title:feed_title content

(* === AUTHENTICATION === *)

(* Cartographic Civic (pass 2): /signup renders through the isolated launch
   wrapper. The form contract is unchanged — POST /signup, Dream CSRF tag,
   username/email/password names and required flags, the required privacy
   checkbox with its exact legal wording, the off-screen 'website' honeypot,
   and the optional Turnstile widget+script — only the chrome and skin moved
   to the approved component classes. The viewer-dependent ?user chrome is
   gone by design (deterministic anonymous top bar), so ?user is accepted
   for signature compatibility and ignored. *)
let signup_form ?user:_ ?error ?turnstile_site_key request =
  let csrf_token = Dream.csrf_tag request in
  (* Server-authored messages only (never user input); rendered as the flat
     rejected notice above the card — no toast, no animation. *)
  let error_html = match error with
    | None -> ""
    | Some msg -> Printf.sprintf "<p class='notice notice--rejected launch-auth__alert'>%s</p>" msg
  in
  (* Turnstile widget is rendered only when a site key is configured. The site key
     is public; still escape it as defense-in-depth. The challenge needs JS to
     solve, but the surrounding SSR form is unaffected when JS is off. *)
  let turnstile_widget, turnstile_script = match turnstile_site_key with
    | None -> "", ""
    | Some key ->
        Printf.sprintf "<div class='cf-turnstile' data-sitekey='%s'></div>" (Components.html_escape key),
        "<script src='https://challenges.cloudflare.com/turnstile/v0/api.js' async defer></script>"
  in
  let content = Printf.sprintf "
        <div class='auth'>
          <div class='auth__head'>
            <img class='auth__mark' src='/static/images/logo-mark.svg' alt=''>
            <h1 class='auth__title'>Create an account</h1>
            <p class='auth__sub'>One account for every community on Earde. No GitHub required.</p>
          </div>
          %s
          <form action='/signup' method='POST' class='auth__card'>
            %s

            <div class='field'>
                <label class='label' for='su-username'>Username</label>
                <div class='input-group'>
                    <span class='input-group__prefix'>u/</span>
                    <input class='input input--mono input--inset' type='text' id='su-username' name='username' required>
                </div>
                <p class='hint'>3&#8211;30 characters</p>
            </div>

            <div class='field'>
                <label class='label' for='su-email'>Email</label>
                <input class='input input--inset' type='email' id='su-email' name='email' required>
            </div>

            <div class='field'>
                <label class='label' for='su-password'>Password</label>
                <input class='input input--inset' type='password' id='su-password' name='password' required>
                <p class='hint'>At least 8 characters.</p>
            </div>

            <!-- Honeypot: positioned off-screen so humans never see or fill it; a non-empty
                 'website' on POST marks the submission as a bot and is silently dropped. -->
            <div style='position:absolute;left:-9999px;top:-9999px;height:0;width:0;overflow:hidden' aria-hidden='true'>
                <label for='website'>Leave this field empty</label>
                <input type='text' id='website' name='website' tabindex='-1' autocomplete='off'>
            </div>

            <label class='check launch-auth__legal' for='privacy'>
                <input id='privacy' name='privacy' type='checkbox' required>
                <span>I agree to the <a href='/privacy' target='_blank'>Privacy Policy</a> and consent to data processing.</span>
            </label>

            %s

            <button type='submit' class='btn btn--primary btn--block launch-auth__submit'>Create account</button>
          </form>
          %s
          <p class='notice launch-auth__notice'>Maintainers: create your account first, then <a href='/bring'>connect a project through GitHub</a>.</p>
          <p class='auth__foot'>Already have an account? <a href='/login'>Log in &#8594;</a></p>
        </div>"
    error_html csrf_token turnstile_widget turnstile_script
  in
  Components.launch_auth_page ~request ~page_class:"launch-signup"
    ~title:"Create an account" ~content ()

(* Cartographic Civic (pass 2): /login through the same launch wrapper. The
   form contract is unchanged — POST /login, Dream CSRF tag, 'identifier' and
   'password' names with their required flags, /forgot-password link — and no
   remember-me is added (unsupported). Failures still render through the
   legacy msg_page, untouched by this pass. ?user ignored as in signup_form. *)
let login_form ?user:_ request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
        <div class='auth'>
          <div class='auth__head'>
            <img class='auth__mark' src='/static/images/logo-mark.svg' alt=''>
            <h1 class='auth__title'>Log in</h1>
            <p class='auth__sub'>Reading is open to everyone. Log in to post, vote and join communities.</p>
          </div>
          <form action='/login' method='POST' class='auth__card'>
            %s
            <div class='field'>
                <label class='label' for='li-identifier'>Username or email</label>
                <input class='input input--inset' type='text' id='li-identifier' name='identifier' required>
            </div>
            <div class='field launch-auth__field-last'>
                <label class='label' for='li-password'>Password</label>
                <input class='input input--inset' type='password' id='li-password' name='password' required>
            </div>
            <div class='launch-auth__meta'>
                <a href='/forgot-password' tabindex='-1'>Forgot password?</a>
            </div>
            <button type='submit' class='btn btn--primary btn--block launch-auth__submit'>Log in</button>
          </form>
          <p class='notice launch-auth__notice'>GitHub is only needed to <b>connect an open-source project</b>. Members read, join and chat with an Earde account alone.</p>
          <p class='auth__foot'>No account? <a href='/signup'>Create one &#8594;</a></p>
        </div>"
    csrf_token
  in
  Components.launch_auth_page ~request ~page_class:"launch-login"
    ~title:"Log in" ~content ()

let forgot_password_page request =
  let csrf_token = Dream.csrf_tag request in
  let card = Printf.sprintf "
        <h1 class='auth-title'>Forgot password?</h1>
        <p class='auth-sub'>Enter your email address and we'll send you a reset link.</p>
        <form action='/forgot-password' method='POST' class='auth-form'>
            %s
            <div class='auth-field'>
                <label class='auth-label' for='fp-email'>Email address</label>
                <input class='auth-input' type='email' id='fp-email' name='email' required placeholder='you@example.com'>
            </div>
            <button type='submit' class='auth-btn'>Send reset link</button>
        </form>
        <div class='auth-foot'><a href='/login' class='auth-link'>Back to login</a></div>"
    csrf_token
  in Components.auth_page ~noindex:true ~request ~title:"Forgot Password" ~card ()

let reset_password_page ~token ?error request =
  let csrf_token = Dream.csrf_tag request in
  let error_html = match error with
    | None -> ""
    | Some msg -> Printf.sprintf "<div class='auth-alert auth-alert--error'>%s</div>" msg
  in
  let card = Printf.sprintf "
        <h1 class='auth-title'>Set new password</h1>
        <p class='auth-sub'>Enter a new password for your account.</p>
        %s
        <form action='/reset-password' method='POST' class='auth-form'>
            %s
            <input type='hidden' name='token' value='%s'>
            <div class='auth-field'>
                <label class='auth-label' for='rp-password'>New password</label>
                <input class='auth-input' type='password' id='rp-password' name='password' required minlength='8'>
            </div>
            <div class='auth-field'>
                <label class='auth-label' for='rp-confirm'>Confirm new password</label>
                <input class='auth-input' type='password' id='rp-confirm' name='confirm_password' required minlength='8'>
            </div>
            <button type='submit' class='auth-btn'>Reset password</button>
        </form>"
    error_html csrf_token token
  in Components.auth_page ~noindex:true ~request ~title:"Reset Password" ~card ()

(* === COMMUNITY === *)

let new_community_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf {html|
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
          <h1 class='create-title'>Start a community</h1>
          <p class='create-sub'>Every community comes with a <strong>#general</strong> live chat channel and a <strong>General</strong> forum section. Add more below — you can always change things later.</p>
        </div>

        <form action='/communities' method='POST' class='create-form' id='new-community-form'>
            %s
            <!-- counts drive the server-side row loops; renumbered by JS on add/remove -->
            <input type='hidden' name='section_count' id='section_count' value='0'>
            <input type='hidden' name='channel_count' id='channel_count' value='0'>

            <div class='create-field'>
                <label class='create-label'>Community name <span class='req'>*</span></label>
                <input type='text' name='name' required class='create-input' placeholder='e.g., Italian Cuisine'>
            </div>

            <div class='create-field'>
                <label class='create-label'>URL slug <span class='req'>*</span></label>
                <div class='create-slug'>
                    <span class='create-slug-prefix'>/c/</span>
                    <input type='text' name='slug' required class='create-input' placeholder='italian-cuisine'>
                </div>
                <p class='create-hint'>Lowercase, no spaces. This is the community's public address.</p>
            </div>

            <div class='create-field'>
                <label class='create-label'>Description</label>
                <textarea name='description' class='create-textarea' placeholder='What is this community about?'></textarea>
            </div>

            <!-- Live chat channels (#): #general is always created; extras are optional -->
            <div class='create-group'>
                <div class='create-group-head'>
                    <div class='create-group-title'><span class='sigil'>#</span> Live chat channels</div>
                    <p class='create-group-desc'>Real-time chat rooms. Messages can later be promoted into permanent forum threads.</p>
                </div>
                <div class='create-chip'>
                    <span class='create-chip-sigil'>#</span>
                    <span class='create-chip-name'>general</span>
                    <span class='create-chip-lock'>Default</span>
                </div>
                <div id='channels-list' class='create-rows'></div>
                <button type='button' class='create-add' onclick='addChannel()'>+ Add channel</button>
            </div>

            <!-- Forum / thread sections (§): General is always created; extras are optional -->
            <div class='create-group'>
                <div class='create-group-head'>
                    <div class='create-group-title'><span class='sigil'>§</span> Forum sections</div>
                    <p class='create-group-desc'>Durable, indexed discussion areas, each with its own feed and sorting.</p>
                </div>
                <div class='create-chip'>
                    <span class='create-chip-sigil'>§</span>
                    <span class='create-chip-name'>General</span>
                    <span class='create-chip-lock'>Default</span>
                </div>
                <div id='sections-list' class='create-rows'></div>
                <button type='button' class='create-add' onclick='addSection()'>+ Add section</button>
            </div>

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Create community</button>
            </div>
        </form>
      </div>
    </div>

    <script>
    /* Rows are renumbered to a contiguous 1..N on every add/remove so the field
       names (channel_name_N / section_name_N) always match the *_count the server
       loops over — removal order can never leave a gap the handler would skip. */
    function renumberChannels() {
        var rows = document.querySelectorAll('#channels-list > .create-row');
        rows.forEach(function(row, i) {
            row.querySelector('input').name = 'channel_name_' + (i + 1);
        });
        document.getElementById('channel_count').value = rows.length;
    }

    function addChannel() {
        var row = document.createElement('div');
        row.className = 'create-row';
        row.innerHTML =
            '<span class="create-row-sigil">#</span>'
            + '<input type="text" placeholder="channel-name" class="create-input create-row-name">'
            + '<button type="button" class="create-row-remove" title="Remove"'
            + ' onclick="this.closest(\'.create-row\').remove(); renumberChannels();">&times;</button>';
        document.getElementById('channels-list').appendChild(row);
        renumberChannels();
    }

    function buildSortSelect() {
        return '<select class="create-select create-row-sort">'
            + '<option value="new">New</option>'
            + '<option value="hot">Hot</option>'
            + '<option value="top">Top</option>'
            + '<option value="active">Active</option>'
            + '</select>';
    }

    function renumberSections() {
        var rows = document.querySelectorAll('#sections-list > .create-row');
        rows.forEach(function(row, i) {
            var idx = i + 1;
            var inputs = row.querySelectorAll('input');
            inputs[0].name = 'section_name_' + idx;
            inputs[1].name = 'section_desc_' + idx;
            row.querySelector('select').name = 'section_sort_' + idx;
        });
        document.getElementById('section_count').value = rows.length;
    }

    function addSection() {
        var row = document.createElement('div');
        row.className = 'create-row';
        row.innerHTML =
            '<span class="create-row-sigil">&sect;</span>'
            + '<input type="text" placeholder="Section name" class="create-input create-row-name">'
            + '<input type="text" placeholder="Description (optional)" class="create-input create-row-desc">'
            + buildSortSelect()
            + '<button type="button" class="create-row-remove" title="Remove"'
            + ' onclick="this.closest(\'.create-row\').remove(); renumberSections();">&times;</button>';
        document.getElementById('sections-list').appendChild(row);
        renumberSections();
    }
    </script>
    |html}
  csrf_token
  in
  Components.create_page ?user ~request ~title:"New Community" ~body:content ()

(* [connected_projects] is the pre-rendered Connected-projects fragment supplied by the
   community route (Community_connected_projects_pages), or "" when the community has no
   accepted project home — and always "" on the section feeds, which are not /c/:slug.
   Defaulting to "" keeps every existing caller unchanged. *)
(* Launch sidebar for the flat (sections_enabled = false) community home — the
   pass-8/9 grammar reduced to what a single-feed community really has: the
   identity head, the one Feed surface (always current — /c/:slug IS the feed),
   the factual private marker, and the Network group with the intentionally
   public moderation log plus Settings under the exact mod-or-admin gate the
   settings handler enforces. No Live or Knowledge groups: a flat community has
   no channel or section surfaces, and nothing is invented. Home requests,
   Reports and Manage moderators stay off this sidebar (the settings/report
   suites count those links per document); Manage moderators keeps its legacy
   placement inside the Moderators panel instead. *)
let launch_flat_community_sidebar ~(community : community) ~can_manage () =
  let esc = Components.html_escape in
  let slug = esc community.slug in
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
               (Components.launch_tile_color community.slug) (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>" src)
    | _ ->
        Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
          (Components.launch_tile_color community.slug) (esc tile_glyph)
  in
  let side_head =
    Printf.sprintf
      "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
      slug side_face (esc community.name) slug
  in
  let nav_feed =
    Printf.sprintf
      "<a class='navitem navitem--pad navitem--active' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Feed</a>"
      slug
  in
  let vis_note =
    if community.visibility = Db.Community_private then
      "<div class='launch-side-vis'>private community</div>"
    else ""
  in
  let nav_network =
    "<p class='kicker sidebar__group'>Network</p>"
    ^ Printf.sprintf
        "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
        slug
    ^ (if can_manage then
         Printf.sprintf
           "<a class='navitem navitem--pad' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
           slug
       else "")
  in
  Printf.sprintf
    "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s</div></aside>"
    (esc community.name) side_head nav_feed vis_note nav_network

(* /c/:slug (flat, sections_enabled = false) — the single-feed community home,
   now on the Cartographic Civic launch chrome (Components.launch_community_page:
   earde.css only, no Tailwind/Google Fonts). This stays a wrapper-and-CSS
   conversion of the legacy simple feed, not a structured community: one feed,
   no channels, no sections, no invented hierarchy. Every contract is the
   pre-launch behavior reskinned:
   - the feed rows and the empty state come from the untouched shared
     Components.render_post / legacy strings byte-for-byte (their DOM is the
     optimistic-vote, confirmModal, dialog and ph-mask contract shared with the
     legacy / and /all feeds) and are skinned purely by the route-scoped CSS;
   - sort (?sort=hot|new|top, unknown→hot upstream) and pagination
     (?sort=&page=, "Page N", prev iff page>1, next iff a full 20-row page)
     keep their exact URLs and grammar;
   - the /join and /leave POSTs keep their exact fields and presence rules
     (anon: none; member: Leave; non-member: Join unless private);
   - + New post keeps the real /new-post?community= destination (still legacy);
   - Settings (mod/admin), Manage moderators (top-mod/admin), the public
     modlog and the top-mod/admin downvote toggle keep their gates and routes;
   - the pre-rendered ccp-* connected-projects fragment is spliced verbatim
     (its markup is pinned by the fragment suites) and restyled by CSS only. *)
let community_page ?user ?(noindex=false) ?(connected_projects="") ~is_member ~is_current_user_mod ~is_current_user_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities user_votes current_page sort_mode (community : community) (posts : post list) request =
  let csrf_token = Dream.csrf_tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let esc = Components.html_escape in
  let slug = esc community.slug in
  (* Fed only the legacy left sidebar; the launch shell's global rail owns
     joined-community navigation now. Kept in the signature so the handler
     call site (shared with the structured branch's data load) stays intact. *)
  ignore (moderated_communities : community list);
  let has_next = List.length posts = 20 in

  let posts_html =
    if posts = [] then "<div class='bg-gray-50 p-12 text-center rounded-xl border border-dashed border-[#D0C9BC] text-gray-500'>No posts yet. Be the first to share something!</div>"
    else String.concat "\n" (List.map (Components.render_post ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames request user_votes) posts)
  in

  let base_url = Printf.sprintf "/c/%s" slug in

  (* Sort chips — the exact hrefs and accepted values of the legacy tabs
     (hot/new/top; the handler maps anything else to hot). *)
  let chip mode label =
    let cls = if mode = sort_mode then "chip chip--active" else "chip" in
    Printf.sprintf "<a class='%s' href='%s?sort=%s'>%s</a>" cls base_url mode label
  in
  let sort_bar =
    Printf.sprintf
      "<div class='tabbar launch-flat-sortbar'><div class='chips' aria-label='Sort'>%s%s%s</div></div>"
      (chip "hot" "Hot") (chip "new" "New") (chip "top" "Top")
  in

  (* Pager — same ?sort=&page= URLs and the same presence rules as before. *)
  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a class='btn btn--secondary btn--sm' href='%s?sort=%s&page=%d'>&larr; Previous</a>" base_url sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a class='btn btn--secondary btn--sm' href='%s?sort=%s&page=%d'>Next &rarr;</a>" base_url sort_mode (current_page + 1) in
  let pager = Printf.sprintf "<div class='pager launch-pager'>%s<span>Page %d</span>%s</div>" prev_btn current_page next_btn in

  let membership_btn =
    match user with
    | None -> ""
    | Some _ ->
        if is_member then Printf.sprintf "<form action='/leave' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--secondary btn--block launch-leave'>Leave</button></form>" csrf_token community.id community.slug
        (* No self-serve join for private communities (Slice C): a non-member who can see this
           page is a mod/admin; show no misleading Join button (the /join route 404s anyway). *)
        else if community.visibility = Db.Community_private then ""
        else Printf.sprintf "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--primary btn--block'>Join</button></form>" csrf_token community.id community.slug
  in

  (* Admins see the settings entry without needing mod status — global authority. *)
  let settings_btn =
    if is_current_user_mod || is_admin then
      Printf.sprintf "<a class='btn btn--secondary' href='/c/%s/settings'>&#9881; Settings</a>" slug
    else ""
  in

  (* Create Post shortcut keeps its real legacy destination and query parameter
     (/new-post stays a legacy page this pass). Logged-in viewers only. *)
  let create_post_btn =
    match user with
    | None -> ""
    | Some _ ->
        Printf.sprintf "<a class='btn btn--primary' href='/new-post?community=%s'>+ New post</a>" community.slug
  in

  (* Community face: avatar image when set and safe, else the launch tile glyph
     on the deterministic palette tone — same fallback as the rail and sidebar. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let face size_cls =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar %s' style='background:%s'>%s</span>"
               size_cls (Components.launch_tile_color community.slug) (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar %s'><img class='launch-avatar-img' src='%s' alt=''></span>"
               size_cls src)
    | _ ->
        Printf.sprintf "<span class='avatar %s' style='background:%s'>%s</span>"
          size_cls (Components.launch_tile_color community.slug) (esc tile_glyph)
  in

  (* Optional banner — rendered only when a safe banner_url is present; no
     gradient placeholder (launch grammar, matching the structured overview). *)
  let banner_html =
    match community.banner_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" -> ""
         | src -> Printf.sprintf "<div class='launch-banner'><img src='%s' class='launch-banner-img' alt=''></div>" src)
    | _ -> ""
  in

  (* Header band. Badges are facts the record already states; public is
     unmarked. No stats line — this page loads no community-wide counts and
     none are fabricated. *)
  let badges =
    (if community.is_network_community then
       " <span class='badge badge--network badge--lg'>&#9672; Network community</span>"
     else "")
    ^ (if community.visibility = Db.Community_private then
         " <span class='badge badge--plain badge--lg'>Private</span>"
       else "")
  in
  let actions = create_post_btn ^ membership_btn ^ settings_btn in
  let chead =
    Printf.sprintf
      "<div class='chead'><div class='chead__row'>%s<div class='launch-chead-id'>\
       <div class='titleline'><h1 class='chead__title'>%s</h1><span class='chead__slug'>/c/%s</span>%s</div>\
       <p class='chead__desc'>%s</p>\
       </div>%s</div></div>"
      (face "avatar--62") (esc community.name) slug badges
      (esc (Option.value ~default:"No description." community.description))
      (if actions = "" then "" else Printf.sprintf "<div class='chead__actions'>%s</div>" actions)
  in

  (* Moderators panel. Manage moderators keeps its legacy placement here and
     its exact top-mod-or-admin gate — regular mods cannot appoint or demote
     peers, preventing collusion against the council. *)
  let manage_mods_link =
    if is_current_user_top_mod || is_admin then
      Printf.sprintf "<div class='launch-manage-mods'><a class='btn--link mono' href='/c/%s/manage-mods'>Manage moderators &rarr;</a></div>" slug
    else ""
  in
  let mods_inner =
    if mod_usernames = [] then "<div class='empty--inline'>No moderators yet.</div>"
    else
      Printf.sprintf "<div class='launch-mods'>%s</div>"
        (String.concat "" (List.map (fun u ->
          let initial =
            if String.length u > 0 then esc (String.sub (String.uppercase_ascii u) 0 1) else "?" in
          Printf.sprintf
            "<a class='launch-modrow' href='/u/%s'><span class='avatar avatar--22 avatar--mod'>%s</span><span class='launch-mod-name'>u/%s</span></a>"
            (esc u) initial (esc u)) mod_usernames))
  in
  let mods_panel =
    Printf.sprintf
      "<section class='panel'><div class='section-head'><span class='kicker'>Moderators</span></div>%s%s</section>"
      mods_inner manage_mods_link
  in

  let rules_panel = match community.rules with
    | Some r when r <> "" ->
        Printf.sprintf
          "<section class='panel'><div class='section-head'><span class='kicker'>Rules</span></div><div class='panel__body launch-rules'>%s</div></section>"
          (esc r)
    | _ -> ""
  in

  (* Toggle downvotes: exposed only to top_mod/admin to prevent vote manipulation arms races
     by regular mods who have less community-wide accountability. Same POST
     route and allow_downvotes field as before. *)
  let mod_tools_panel =
    if is_current_user_top_mod || is_admin then
      let (next_val, label, state) =
        if community.allow_downvotes then ("false", "Disable downvotes", "enabled")
        else ("true", "Enable downvotes", "disabled")
      in
      Printf.sprintf
        "<section class='panel'><div class='section-head'><span class='kicker'>Mod tools</span></div><div class='panel__body'><p class='launch-modtool-state'>Downvotes are currently %s.</p><form action='/c/%s/toggle_downvotes' method='POST'>%s<input type='hidden' name='allow_downvotes' value='%s'><button type='submit' class='btn btn--secondary btn--sm'>%s</button></form></div></section>"
        state slug csrf_token next_val label
    else ""
  in

  let sidebar =
    launch_flat_community_sidebar ~community
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* Single-feed body: the ledger column plus the factual side panels. The
     pre-rendered ccp-* fragment closes the side stack, spliced verbatim. *)
  let content =
    Printf.sprintf
      "<div class='scroll'>%s%s<div class='container launch-flat-body'><div class='two-col'><div class='stack'>%s<div class='launch-flat-ledger'>%s</div>%s</div><div class='stack stack--sm'>%s%s%s%s</div></div></div></div>"
      banner_html chead
      sort_bar posts_html pager
      mods_panel rules_panel mod_tools_panel connected_projects
  in
  (* The byte-pinned rows show Share to every viewer, but only member
     documents carry the full behavior script (which defines copyPostLink).
     Guests get the share-only script — same single copyPostLink source, no
     notification fetch — so the control works in both viewer states and each
     document holds exactly one definition. *)
  let head_extra = match user with
    | None -> Components.launch_share_script
    | Some _ -> ""
  in
  Components.launch_community_page ?user ~noindex ~request ~head_extra
    ~rail_communities:user_communities ~community ~sidebar
    ~page_class:"launch-flat-community" ~title:community.name ~content ()

(* The forum-section feed inside a structured community, and (below) the canonical thread
   view — the pass-10 knowledge routes, now on the Cartographic Civic launch chrome. Kept as
   separate functions (not folded into community_page) so the simple-community feed and the
   section feed can diverge in chrome without one breaking the other. SSR-only — every link
   works with JS disabled; the page JS only enhances. *)
(* Launch community sidebar for the knowledge routes (section + thread), in the exact
   pass-8/9 grammar: identity head, Overview link, factual visibility marker, Live channels
   (never active here — these are forum surfaces), Knowledge sections with the current/parent
   section active, and the intentionally public moderation log. Settings renders only under
   the is_mod-or-admin gate its handler enforces; Home requests needs the top-mod authority
   the section/thread handlers never load, so it is not rendered — nothing is invented. The
   virtual Uncategorized feed is appended (active) only when the viewer is on it: the handler
   404s that route unless real orphaned content exists, so the entry is always backed by
   data. Archived channels are hidden, mirroring the pre-launch Channels nav group.
   [settings_active] marks the Settings entry current — used only by the settings route
   (pass 11A), whose surface already re-proved the can_manage gate before rendering.
   [home_requests_active] renders the Home requests entry (active) — used only by the
   review-queue route (pass 11B), whose read model already re-proved the top-mod/admin
   gate in SQL before anything renders; every other route keeps the entry absent, so
   the settings suites' exactly-one-queue-link count per document stays true.
   [show_visibility_note] defaults to the existing factual marker; the review queue
   passes false because that surface never names an ineligibility reason
   (private/draft/legacy) anywhere in its document.
   [moderation_log_active] marks the always-present Moderation log entry current —
   used only by the modlog route (pass 14A), which is public by design, so the
   entry itself renders for every viewer exactly as before.
   [reports_active] renders the Reports entry (active) — used only by the
   report-queue route (pass 14B), whose handler already re-proved the M/TM/A
   gate before anything renders; every other route keeps the entry absent, so
   the queue link never shows to a viewer who cannot open the route. *)
let launch_knowledge_sidebar ~(community : community) ~(channels : channel list)
    ~(sections : community_section list) ?active_section_slug
    ?(append_uncategorized = false) ?(settings_active = false)
    ?(home_requests_active = false) ?(moderation_log_active = false)
    ?(reports_active = false) ?(manage_moderators_active = false)
    ?(show_visibility_note = true) ~can_manage () =
  let esc = Components.html_escape in
  let slug = esc community.slug in
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
               (Components.launch_tile_color community.slug) (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>" src)
    | _ ->
        Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
          (Components.launch_tile_color community.slug) (esc tile_glyph)
  in
  let side_head =
    Printf.sprintf
      "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
      slug side_face (esc community.name) slug
  in
  let nav_overview =
    Printf.sprintf
      "<a class='navitem navitem--pad' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
      slug
  in
  let vis_note =
    if show_visibility_note && community.visibility = Db.Community_private then
      "<div class='launch-side-vis'>private community</div>"
    else ""
  in
  let live_channels = List.filter (fun (c : channel) -> not c.is_archived) channels in
  let nav_live =
    if live_channels = [] then ""
    else
      "<p class='kicker sidebar__group'>Live</p>"
      ^ String.concat "" (List.map (fun (c : channel) ->
          Printf.sprintf
            "<a class='navitem' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
            slug (esc c.slug) (esc c.slug))
          live_channels)
  in
  let section_item (s : community_section) =
    let cls =
      if active_section_slug = Some s.slug then "navitem navitem--active" else "navitem" in
    Printf.sprintf
      "<a class='%s' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s</a>"
      cls slug (esc s.slug) (esc s.name)
  in
  let knowledge_items =
    List.map section_item sections
    @ (if append_uncategorized then
         [ Printf.sprintf
             "<a class='navitem navitem--active' href='/c/%s/s/uncategorized'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>Uncategorized</a>"
             slug ]
       else [])
  in
  let nav_knowledge =
    if knowledge_items = [] then ""
    else "<p class='kicker sidebar__group'>Knowledge</p>" ^ String.concat "" knowledge_items
  in
  let nav_network =
    "<p class='kicker sidebar__group'>Network</p>"
    ^ Printf.sprintf
        "<a class='navitem navitem--pad%s' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
        (if moderation_log_active then " navitem--active" else "") slug
    ^ (if home_requests_active then
         (* Same slot and grammar as the overview's Home requests entry;
            rendered only by the queue route whose viewer the read model
            already proved top_mod-or-durable-admin. *)
         Printf.sprintf
           "<a class='navitem navitem--pad navitem--active' href='/c/%s/project-home-requests'><span class='navitem__sigil navitem__sigil--box navitem__sigil--project'>&#9672;</span>Home requests</a>"
           slug
       else "")
    ^ (if reports_active then
         (* Rendered only by the report-queue route, whose handler proved the
            viewer M/TM/A before this document exists — never a render-time
            authority decision of its own. *)
         Printf.sprintf
           "<a class='navitem navitem--pad navitem--active' href='/c/%s/reports'><span class='navitem__sigil navitem__sigil--box'>&#9873;</span>Reports</a>"
           slug
       else "")
    ^ (if manage_moderators_active then
         (* Rendered only by the manage-mods route, whose handler proved the
            viewer TM/A before this document exists — never a render-time
            authority decision of its own. *)
         Printf.sprintf
           "<a class='navitem navitem--pad navitem--active' href='/c/%s/manage-mods'><span class='navitem__sigil navitem__sigil--box'>&#9878;</span>Manage moderators</a>"
           slug
       else "")
    ^ (if can_manage then
         Printf.sprintf
           "<a class='navitem navitem--pad%s' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
           (if settings_active then " navitem--active" else "") slug
       else "")
  in
  Printf.sprintf
    "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
    (esc community.name) side_head nav_overview vis_note nav_live nav_knowledge nav_network

let community_section_shell_page ?user ?(noindex=false) ?thread_count ?last_activity ~is_current_user_mod ~mod_usernames ~admin_usernames
    ~banned_usernames ~(rail_communities : community list) ~(channels : channel list)
    ~(sections : community_section list)
    ~(section : community_section) ~user_votes ~current_page ~sort_mode
    ~(community : community) ~(posts : post list) request =
  let esc = Components.html_escape in
  (* base_url drives the sort tabs, the New-thread link, and pagination — all stay on the section URL. *)
  let base_url = Printf.sprintf "/c/%s/s/%s" (esc community.slug) (esc section.slug) in

  (* Launch community sidebar (pass-8/9 grammar): the current section is active; the virtual
     Uncategorized feed isn't a real section row, so it is appended (active) only when we're
     on it — keeps it highlighted without an extra orphaned-count query just to decorate the
     sidebar. Settings gate mirrors the overview: the render-time check is visibility only,
     the settings handler re-checks authority. *)
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ?active_section_slug:(if section.slug = "uncategorized" then None else Some section.slug)
      ~append_uncategorized:(section.slug = "uncategorized")
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* New-thread action: pre-selects this section; suppressed for anon users and the virtual
     Uncategorized feed (users cannot post directly into it). Shared by header + right rail. *)
  let new_thread_btn ?(cls="btn sm primary") () =
    match user with
    | None -> ""
    | Some _ ->
        if section.slug = "uncategorized" then ""
        else Printf.sprintf "<a href='/new-post?community=%s&section=%s' class='%s'>+ New thread</a>"
          (esc community.slug) (esc section.slug) cls
  in

  (* Sort tabs — Hot/New/Top/Active, active highlighted. No counts (they would be fabricated) and
     no Unanswered (no such state). Preserves the ?sort override + the section's default sort. *)
  let tab mode label =
    let cls = if mode = sort_mode then " class='active'" else "" in
    Printf.sprintf "<a%s href='%s?sort=%s'>%s</a>" cls base_url mode label
  in
  let ftabs = Printf.sprintf "<div class='ftabs'>%s%s%s%s</div>"
    (tab "hot" "Hot") (tab "new" "New") (tab "top" "Top") (tab "active" "Active")
  in

  (* Section header: breadcrumb, prominent § title, optional description, New thread, sort tabs.
     It lives outside cs-main-body so it stays put while the thread list scrolls under it. *)
  let desc_html = match section.description with
    | Some d when String.trim d <> "" -> Printf.sprintf "<div class='fh-desc'>%s</div>" (esc d)
    | _ -> ""
  in
  let forum_head = Printf.sprintf "
    <div class='forum-head'>
        <div class='fh-crumb'><a href='/c/%s'>/c/%s</a> / <b>§ %s</b></div>
        <div class='fh-top'>
            <div><div class='fh-title'><span class='sec'>§</span> %s</div>%s</div>
            <div class='fh-actions'>%s</div>
        </div>
        %s
    </div>"
    (esc community.slug) (esc community.slug) (esc section.name)
    (esc section.name) desc_html
    (new_thread_btn ())
    ftabs
  in

  let posts_html =
    if posts = [] then "<div class='cs-empty'>No threads here yet — start the first one.</div>"
    else String.concat "\n" (List.map (Components.render_forum_row ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames request user_votes) posts)
  in

  let has_next = List.length posts = 20 in
  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a class='btn sm' href='%s?sort=%s&page=%d'>&larr; Previous</a>" base_url sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a class='btn sm' href='%s?sort=%s&page=%d'>Next &rarr;</a>" base_url sort_mode (current_page + 1) in
  let pager = Printf.sprintf "<div class='cs-pager'><div>%s</div><span class='cs-pager-n'>Page %d</span><div>%s</div></div>" prev_btn current_page next_btn in

  (* cs-flush zeroes cs-main-body's gutter so thread rows can span full width (their own padding);
     forum_head sits outside the scroll area so it stays put while the list scrolls. *)
  let main = Printf.sprintf "%s<div class='cs-main-body cs-flush'>%s%s</div>" forum_head posts_html pager in

  (* Right rail — real section metadata only. Rows whose data we don't have (thread count / last
     activity) are simply omitted; nothing here is fabricated. *)
  let cap s = if s = "" then s else String.make 1 (Char.uppercase_ascii s.[0]) ^ String.sub s 1 (String.length s - 1) in
  let threads_row = match thread_count with
    | Some n -> Printf.sprintf "<tr><td>Threads</td><td class='num'>%d</td></tr>" n
    | None -> ""
  in
  let activity_row = match last_activity with
    | Some ts when String.trim ts <> "" -> Printf.sprintf "<tr><td>Last activity</td><td class='num'>%s</td></tr>" (esc (Components.time_ago ts))
    | _ -> ""
  in
  let stats_cta = match new_thread_btn ~cls:"btn sm primary block" () with "" -> "" | b -> Printf.sprintf "<div class='ca-cta'>%s</div>" b in
  let stats_block = Printf.sprintf "
    <div class='ca-block'>
        <div class='ca-label'>Section · indexed</div>
        <table class='ca-grid'><tbody>%s<tr><td>Default sort</td><td class='num'>%s</td></tr>%s</tbody></table>
        %s
    </div>"
    threads_row (esc (cap section.default_sort)) activity_row stats_cta
  in
  (* community.rules are community-wide, NOT per-section — label them honestly. Sections have no
     rules field of their own yet, so we never imply they do; omitted entirely when unset. *)
  let rules_block = match community.rules with
    | Some r when String.trim r <> "" ->
        Printf.sprintf "<div class='ca-block'><div class='ca-label'>Community rules</div><div class='ca-rules'>%s</div></div>" (esc r)
    | _ -> ""
  in
  let mods_block =
    if mod_usernames = [] then ""
    else
      let rows = String.concat "" (List.map (fun u ->
        Printf.sprintf "<div class='member'><a href='/u/%s'>%s</a><span class='role mod'>MOD</span></div>" (esc u) (esc u)) mod_usernames)
      in
      Printf.sprintf "<div class='ca-block'><div class='ca-label'>Moderators</div>%s</div>" rows
  in
  let right_pane = stats_block ^ rules_block ^ mods_block in

  let title = Printf.sprintf "%s · %s" section.name community.name in
  (* The complete <main> element, built here so the launch wrapper can never interpose a
     box: .cs-main is a flex column whose forum head and scrolling body MUST stay direct
     children, and for a private community the ph-no-capture replay guard rides on this
     element itself (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    Printf.sprintf "<main class='%s'>%s</main>"
      (if community.visibility = Db.Community_private then "cs-main ph-no-capture" else "cs-main")
      main
  in
  let aside = Printf.sprintf "<aside class='aside'>%s</aside>" right_pane in
  Components.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~aside ~community ~sidebar ~page_class:"launch-community-section" ~title ~main_el ()

(* /feed — the global Feed surface, now the first App route on the launch
   chrome (Components.launch_app_page: earde.css only, no shell.css/Tailwind).
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
    ~(rail_communities : community list) ~user_votes ~current_page
    (posts : post list) request =
  let esc = Components.html_escape in

  (* Contact links: reuse the existing founder Telegram link; keep the existing pilot mailto. *)
  let founder_tg = "https://t.me/tolwiz" in
  let mail_pilot    = "mailto:metacirculardispatches@gmail.com?subject=Start%20a%20pilot%20community%20on%20Earde" in

  (* Scope toggle (logged-in only). Each link keeps the current sort. *)
  let scope_tabs =
    if not is_logged_in then ""
    else
      let tab s label =
        let cls = if s = scope then "tab tab--active" else "tab" in
        Printf.sprintf "<a class='%s' href='/feed?scope=%s&sort=%s'>%s</a>" cls s sort_mode label
      in
      Printf.sprintf "<nav class='tabs launch-scope-tabs' aria-label='Feed scope'>%s%s</nav>"
        (tab "following" "Following") (tab "all" "All communities")
  in

  (* Sort chips — keep the current scope. *)
  let chip mode label =
    let cls = if mode = sort_mode then "chip chip--active" else "chip" in
    Printf.sprintf "<a class='%s' href='/feed?scope=%s&sort=%s'>%s</a>" cls scope mode label
  in
  let sort_bar =
    Printf.sprintf
      "<div class='tabbar launch-sort-bar'><div class='chips' aria-label='Sort'>%s%s%s%s</div></div>"
      (chip "hot" "Hot") (chip "new" "New") (chip "top" "Top") (chip "active" "Active")
  in

  let page_head =
    Printf.sprintf
      "<div class='page__head'><div class='page__head-inner'>\
       <h1 class='page__title'>Feed</h1>\
       <p class='page__sub'>Live activity and durable knowledge across the communities you follow.</p>\
       %s%s</div></div>"
      scope_tabs sort_bar
  in

  (* Empty state. Following-empty keeps its two real destinations (public
     threads + the pilot mailto) under the launch .empty idiom. *)
  let posts_html =
    if posts <> [] then
      String.concat "\n" (List.map (Components.render_forum_row ~admin_usernames ~show_context:true request user_votes) posts)
    else if scope = "following" && is_logged_in then
      Printf.sprintf
        "<div class='empty'>\
         <div class='empty__title'>Your feed is empty</div>\
         <p class='empty__body'>You're not following any communities yet. Browse public threads while Earde is early, or start a pilot community.</p>\
         <div class='launch-empty-actions'>\
         <a class='btn btn--secondary btn--sm' href='/feed?scope=all&sort=%s'>All public threads</a>\
         <a class='btn btn--quiet btn--sm' href='%s'>Start a pilot community</a>\
         </div></div>"
        sort_mode mail_pilot
    else
      "<div class='empty'>\
       <div class='empty__title'>No public threads yet.</div>\
       <p class='empty__body'>Once communities post, durable discussions surface here.</p>\
       </div>"
  in

  let has_next = List.length posts = 20 in
  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a class='btn btn--secondary btn--sm' href='/feed?scope=%s&sort=%s&page=%d'>&larr; Previous</a>" scope sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a class='btn btn--secondary btn--sm' href='/feed?scope=%s&sort=%s&page=%d'>Next &rarr;</a>" scope sort_mode (current_page + 1) in
  let pager = Printf.sprintf "<div class='pager launch-pager'>%s<span>Page %d</span>%s</div>" prev_btn current_page next_btn in

  let content =
    Printf.sprintf "%s<div class='scroll'><div class='container'>%s\n%s</div></div>"
      page_head posts_html pager
  in

  (* Right aside — factual static product copy + real page data only: the
     handoff's Bring block, the existing founder/pilot contacts, and the real
     Following list. No Atlas, no fake trends, no fabricated counts. *)
  let bring_block =
    "<div class='aside__block'>\
     <div class='kicker aside__kicker'>Bring your community</div>\
     <p class='aside__text'>Maintain an open-source project? Verify it through GitHub and give it a home on Earde.</p>\
     <a class='btn btn--accent btn--block btn--sm' href='/bring'>Connect a project</a>\
     </div>"
  in
  let founder_block = Printf.sprintf
    "<div class='aside__block'>\
     <div class='kicker aside__kicker'>Earde is early</div>\
     <p class='aside__text'>If you are trying to build a community here, I want to hear what works, what breaks, and what features you need next.</p>\
     <a class='btn btn--secondary btn--block btn--sm' href='%s' target='_blank' rel='noopener noreferrer'>Talk to me!</a>\
     <div class='launch-aside-alt'><a href='mailto:metacirculardispatches@gmail.com'>or email me</a></div>\
     </div>"
    founder_tg
  in
  let pilot_block = Printf.sprintf
    "<div class='aside__block'>\
     <div class='kicker aside__kicker'>Start here</div>\
     <p class='aside__text'>Want to try Earde with your community? I can help you set it up.</p>\
     <a class='btn btn--outline-ochre btn--block btn--sm' href='%s'>Start a pilot community</a>\
     </div>"
    mail_pilot
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
    if rail_communities = [] then ""
    else
      let rows = String.concat "" (List.map (fun (c : community) ->
        Printf.sprintf
          "<a class='navitem' href='/c/%s'><span class='avatar avatar--20' style='background:%s'>%s</span><span class='mono'>/c/%s</span></a>"
          (esc c.slug) (Components.launch_tile_color c.slug) (esc (tile_glyph c.slug)) (esc c.slug)
      ) rail_communities) in
      Printf.sprintf "<div class='aside__block'><div class='kicker aside__kicker'>Following</div>%s</div>" rows
  in
  let aside = bring_block ^ founder_block ^ pilot_block ^ following_block in

  Components.launch_app_page ?user ~request ~rail_communities ~aside
    ~page_class:"launch-feed" ~title:"Feed" ~content ()

(* Pure helpers for "Start thread from chat". Extracted from the handler so the
   title/body prefill, checkbox-id parsing, and channel-row marker classification are
   unit-testable without a DB or a live request (see test/test_earde.ml). Placed here,
   above community_channel_shell_page, because the channel renderer uses
   classify_message_links. UI says "Start thread", never "Promote". *)
module Start_thread = struct
  (* First-sentence-ish title from the seed message: cut at the first . ! ? or newline,
     then hard-cap to ~80 bytes on a word boundary with an ellipsis. Always trimmed; the
     form marks the field required, so an empty seed yields "" and the user types one. *)
  let derive_title (content : string) : string =
    let s = String.trim content in
    let n = String.length s in
    let stop =
      let rec find i =
        if i >= n then n
        else match s.[i] with
          | '.' | '!' | '?' | '\n' | '\r' -> i
          | _ -> find (i + 1)
      in find 0
    in
    let first = String.trim (String.sub s 0 stop) in
    let max_len = 80 in
    if String.length first <= max_len then first
    else begin
      let cut = String.sub first 0 max_len in
      let cut = match String.rindex_opt cut ' ' with
        | Some sp when sp > 40 -> String.sub cut 0 sp
        | _ -> cut
      in
      cut ^ "\xe2\x80\xa6" (* … *)
    end

  (* Checkbox field names are "msg_<id>" (value "on"). Repeated same-name fields don't
     survive Dream.form's assoc list, so one distinct key per candidate is used. Parse
     -> deduped int64 list; non-matching keys and unparseable ids are ignored. *)
  let parse_selected_ids (form_data : (string * string) list) : int64 list =
    let plen = String.length "msg_" in
    List.filter_map (fun (k, _v) ->
      if String.length k > plen && String.sub k 0 plen = "msg_" then
        (try Some (Int64.of_string (String.sub k plen (String.length k - plen)))
         with _ -> None)
      else None
    ) form_data
    |> List.sort_uniq Int64.compare

  (* Channel-row marker for a chat message, decided from all its thread-source links.
     Mk_seed: the message seeds a thread (wins — a message seeds at most one). Mk_referenced
     (target_post_id, title, ref_count): used only as context elsewhere; still startable.
     Mk_no_link: no relation. *)
  type msg_marker =
    | Mk_seed of int * string
    | Mk_referenced of int * string * int
    | Mk_no_link

  (* Pure classification from one message's links [(post_id, title, is_seed)]. Order-
     independent: seed wins; otherwise the highest post_id (most recent) is the link
     target and ref_count is how many threads reference the message. *)
  let classify_message_links (links : (int * string * bool) list) : msg_marker =
    match List.find_opt (fun (_, _, is_seed) -> is_seed) links with
    | Some (pid, title, _) -> Mk_seed (pid, title)
    | None ->
        match links with
        | [] -> Mk_no_link
        | first :: rest ->
            let (pid, title, _) =
              List.fold_left (fun (bp, bt, bs) (pid, title, is_seed) ->
                if pid > bp then (pid, title, is_seed) else (bp, bt, bs))
                first rest
            in
            Mk_referenced (pid, title, List.length links)

  (* Server-side selection guard: keep only ids that are real candidates (in [valid]),
     drop the seed (forced separately), dedup, order chronologically (ids are monotonic),
     then cap context to max_total-1 so seed+context never exceeds max_total. *)
  let normalize_selection ~seed ~max_total ~(valid : int64 list) (selected : int64 list) : int64 list =
    let rec take n = function
      | [] -> []
      | _ when n <= 0 -> []
      | x :: xs -> x :: take (n - 1) xs
    in
    selected
    |> List.filter (fun id -> id <> seed && List.mem id valid)
    |> List.sort_uniq Int64.compare
    |> take (max_total - 1)

  (* ?source_thread=<post_id> reverse-navigation parameter. Strict positive-int parse:
     anything unparseable, zero, or negative reads as "no focus" so a mangled URL renders
     the normal channel page instead of an error. *)
  let parse_source_thread (raw : string option) : int option =
    match raw with
    | None -> None
    | Some s ->
        (match int_of_string_opt (String.trim s) with
         | Some n when n > 0 -> Some n
         | _ -> None)

  (* Comma-joined id list for the data-source-highlight-ids attribute. Digits and commas
     only by construction, so it is attribute-safe without escaping. *)
  let highlight_ids_attr (ids : int64 list) : string =
    String.concat "," (List.map Int64.to_string ids)

  (* Compact display forms of a Postgres timestamp-text ("YYYY-MM-DD HH:MM:SS..."):
     date_of_ts -> "YYYY-MM-DD", minute_of_ts -> "YYYY-MM-DD HH:MM". Pure truncation —
     timestamps are stored UTC and rendered verbatim elsewhere in chat, so no timezone
     math here. Short/odd inputs pass through untouched. *)
  let date_of_ts (ts : string) : string =
    if String.length ts >= 10 then String.sub ts 0 10 else ts

  let minute_of_ts (ts : string) : string =
    if String.length ts >= 16 then String.sub ts 0 16 else ts

  (* Provenance metadata for the promoted-conversation block, derived purely from the
     persisted source rows — never from the curator's editable introduction. Participants
     are distinct non-empty authors of AVAILABLE rows (deleted rows are masked and would
     otherwise all collapse into one fake "" participant). Date range spans all rows. *)
  type source_summary = {
    ss_available : int;
    ss_unavailable : int;
    ss_participants : int;
    ss_date_range : string;
  }

  let summarize_source (msgs : Db.thread_source_msg list) : source_summary =
    let available = List.filter (fun (m : Db.thread_source_msg) -> not m.sm_deleted) msgs in
    let participants =
      List.fold_left (fun acc (m : Db.thread_source_msg) ->
        let a = String.trim m.sm_author in
        if a = "" || List.mem a acc then acc else a :: acc) [] available
    in
    let dates = List.map (fun (m : Db.thread_source_msg) -> date_of_ts m.sm_created_at) msgs in
    let date_range =
      match dates with
      | [] -> ""
      | first :: rest ->
          let last = List.fold_left (fun _ d -> d) first rest in
          if first = last then first else first ^ " \xe2\x80\x93 " ^ last (* – *)
    in
    { ss_available = List.length available;
      ss_unavailable = List.length msgs - List.length available;
      ss_participants = List.length participants;
      ss_date_range = date_range }
end

let community_channel_shell_page ?user ?realtime_token ?(noindex=false) ~is_member ?(can_start=false)
    ?(thread_links : (int64 * int * string * bool) list = [])
    ?(source_focus : (int * string * int64 list) option)
    ~(rail_communities : community list)
    ~(channels : channel list) ~(sections : community_section list)
    ~(channel : channel) ~(messages : (chat_message * string option) list)
    ~(community : community) request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let channel_url = Printf.sprintf "/c/%s/ch/%s" (esc community.slug) (esc channel.slug) in

  (* Launch community sidebar (pass-8 grammar, channel variant): identity
     head, Overview link, factual visibility marker, Live channels with the
     CURRENT channel active, Knowledge sections (this renderer has no
     per-section counts, so none are shown), and the intentionally public
     moderation log. Settings and Home requests need the mod/top-mod
     authority the channel handler never loads, so they are not rendered
     here — nothing is invented. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let side_face =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
               (Components.launch_tile_color community.slug) (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar avatar--32'><img class='launch-avatar-img' src='%s' alt=''></span>" src)
    | _ ->
        Printf.sprintf "<span class='avatar avatar--32' style='background:%s'>%s</span>"
          (Components.launch_tile_color community.slug) (esc tile_glyph)
  in
  let side_head =
    Printf.sprintf
      "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
      (esc community.slug) side_face (esc community.name) (esc community.slug)
  in
  let nav_overview =
    Printf.sprintf
      "<a class='navitem navitem--pad' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
      (esc community.slug)
  in
  let vis_note =
    if community.visibility = Db.Community_private then
      "<div class='launch-side-vis'>private community</div>"
    else ""
  in
  let nav_live =
    if channels = [] then ""
    else
      "<p class='kicker sidebar__group'>Live</p>"
      ^ String.concat "" (List.map (fun (c : channel) ->
          let cls = if c.slug = channel.slug then "navitem navitem--active" else "navitem" in
          Printf.sprintf
            "<a class='%s' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
            cls (esc community.slug) (esc c.slug) (esc c.slug))
          channels)
  in
  let nav_knowledge =
    if sections = [] then ""
    else
      "<p class='kicker sidebar__group'>Knowledge</p>"
      ^ String.concat "" (List.map (fun (s : community_section) ->
          Printf.sprintf
            "<a class='navitem' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s</a>"
            (esc community.slug) (esc s.slug) (esc s.name))
          sections)
  in
  let nav_network =
    "<p class='kicker sidebar__group'>Network</p>"
    ^ Printf.sprintf
        "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
        (esc community.slug)
  in
  let sidebar =
    Printf.sprintf
      "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
      (esc community.name) side_head nav_overview vis_note nav_live nav_knowledge nav_network
  in

  let topic_html = match channel.topic with
    | Some t when t <> "" -> Printf.sprintf "<span class='cs-ch-topic'>%s</span>" (esc t)
    | _ -> ""
  in
  (* Shared cursors are opt-in and community-gated: the server decides whether
     the control exists at all (Features allow-list), so the browser can't
     enable the feature by editing its DOM/URL. Logged-out viewers get no
     control — they have no realtime token to share through. The checkbox
     itself only governs broadcasting; seeing others' cursors needs no opt-in. *)
  let share_control =
    if user <> None && Features.shared_cursors_enabled ~community_slug:community.slug then
      Printf.sprintf
        "<label class='cs-cursor-share' id='chat-cursor-share' data-community-slug='%s'><input type='checkbox' id='chat-cursor-share-toggle'>Share cursor</label>"
        (esc community.slug)
    else ""
  in
  let head = Printf.sprintf
    "<div class='cs-main-head'><span class='cs-hash'>#</span><span>%s</span>%s%s</div>"
    (esc channel.name) topic_html share_control
  in

  (* Stream oldest → newest (the DB read already returns ascending), so newest sits at the
     bottom. Deleted messages are masked; a NULL/unknown author (GDPR tombstone) shows
     "[deleted]". Avatar glyph = first letter of the resolved author. *)
  let render_message ((m : chat_message), (author : string option)) =
    let name = match author with Some u -> u | None -> "[deleted]" in
    let initial =
      if String.length name > 0 && name.[0] <> '['
      then String.uppercase_ascii (String.sub name 0 1) else "?"
    in
    let body =
      if m.deleted_at <> None then "<span class='cs-msg-deleted'>[message deleted]</span>"
      else esc m.content
    in
    (* Per-message thread action, from this message's thread-source links. A seed links
       to its thread ("Thread ->") and suppresses "Start thread". A message only
       referenced as context shows "Referenced in ->" (+N if several) AND still offers
       "Start thread". "Start thread" needs a member who can_start on a non-deleted,
       authored message; the form re-validates every permission server-side. Thread/
       reference links are public (visible to everyone). *)
    let short t = if String.length t > 40 then String.sub t 0 39 ^ "\xe2\x80\xa6" else t in
    let links_for_msg =
      List.filter_map (fun (mid, pid, title, is_seed) ->
        if mid = m.id then Some (pid, title, is_seed) else None) thread_links in
    let marker = Start_thread.classify_message_links links_for_msg in
    (* A message that already seeds a thread is "promoted"; suppress Start thread on it so a
       message can't be promoted twice (the "Thread ->" marker below already links its thread).
       Referenced-only messages (Mk_referenced) are still startable. *)
    let already_promoted = match marker with Start_thread.Mk_seed _ -> true | _ -> false in
    let promote_url = Printf.sprintf "/c/%s/ch/%s/messages/%Ld/start-thread" (esc community.slug) (esc channel.slug) m.id in
    let start_link =
      if can_start && m.deleted_at = None && m.user_id <> None && not already_promoted then
        Printf.sprintf "<a class='cs-msg-start' href='%s' data-promote-url='%s'>Start thread</a>" promote_url promote_url
      else "" in
    (* Minute precision, matching chat_message_json, so SSR rows and rows the
       JS appends later (live, catch-up, composer response) display alike. *)
    let time_text = Start_thread.minute_of_ts m.created_at in
    let time_html =
      if start_link = "" then Printf.sprintf "<span class='cs-msg-time'>%s</span>" (esc time_text)
      else Printf.sprintf "<span class='cs-msg-time-slot'><span class='cs-msg-time'>%s</span>%s</span>" (esc time_text) start_link
    in
    (* Provenance markers stay attached under the message text. Start thread is rendered
       in the meta row beside the timestamp so hover never changes message height. *)
    let action =
      match marker with
      | Start_thread.Mk_seed (post_id, title) ->
          Printf.sprintf "<a class='cs-msg-thread' href='%s'>Started thread &rarr; %s</a>"
            (Components.canonical_thread_path community.slug post_id title) (esc (short title))
      | Start_thread.Mk_referenced (post_id, title, count) ->
          let extra = if count > 1 then Printf.sprintf " <span class='cs-msg-refmore'>+%d</span>" (count - 1) else "" in
          Printf.sprintf "<a class='cs-msg-ref' href='%s'>Included in thread &rarr; %s</a>%s"
            (Components.canonical_thread_path community.slug post_id title) (esc (short title)) extra
      | Start_thread.Mk_no_link -> ""
    in
    let actions_row = if action = "" then "" else Printf.sprintf "<div class='cs-msg-actions'>%s</div>" action in
    (* id='msg-<id>' makes per-message deep links (#msg-…) work with JS off; the reverse-
       navigation highlighter also targets rows through it. *)
    Printf.sprintf
      "<div class='cs-msg' id='msg-%Ld' data-message-id='%Ld' data-has-thread='%s'><div class='cs-msg-avatar'>%s</div><div class='cs-msg-body'><div class='cs-msg-meta'><span class='cs-msg-author'>%s</span>%s</div><div class='cs-msg-text'>%s</div>%s</div></div>"
      m.id m.id (if already_promoted then "true" else "false") (esc initial) (esc name) time_html body actions_row
  in
  let messages_html =
    if messages = [] then
      "<div class='cs-msg-empty'>No messages yet. Be the first to say something.</div>"
    else String.concat "\n" (List.map render_message messages)
  in

  (* Composer is a real <form method=POST> (works with JS off). Three states:
     anon → log in; logged-in non-member → join; member → send. The send form carries
     community_slug + channel_slug (not a raw id) so the handler re-validates ownership. *)
  let composer =
    match user with
    | None ->
        Printf.sprintf "<div class='cs-composer cs-composer-prompt'>Please <a href='/login'>log in</a> to chat in #%s.</div>"
          (esc channel.slug)
    | Some _ when not is_member && community.visibility = Db.Community_private ->
        (* Private community: a non-member viewing this is an authorized mod/admin; they still
           can't chat without membership, but show no self-join button (Slice C). *)
        "<div class='cs-composer cs-composer-prompt'><span>Only members can chat in this private community.</span></div>"
    | Some _ when not is_member ->
        Printf.sprintf "<div class='cs-composer cs-composer-prompt'><span>Join this community to chat.</span><form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='cs-send'>Join &amp; chat</button></form></div>"
          csrf_token community.id channel_url
    | Some _ ->
        Printf.sprintf "<div class='cs-composer'><form action='/messages' method='POST'>%s<input type='hidden' name='community_slug' value='%s'><input type='hidden' name='channel_slug' value='%s'><textarea name='content' rows='1' maxlength='4000' placeholder='Message #%s' required></textarea><button type='submit' class='cs-send'>Send</button></form></div>"
          csrf_token (esc community.slug) (esc channel.slug) (esc channel.slug)
  in

  let realtime_socket_url =
    match Sys.getenv_opt "REALTIME_SOCKET_URL" with
    | Some url when String.trim url <> "" -> String.trim url
    | _ ->
        Logs.warn (fun m ->
            m "REALTIME_SOCKET_URL is not set; live chat websocket disabled for this page");
        ""
  in
  let realtime_signed_token =
    if realtime_socket_url = "" then ""
    else Option.value realtime_token ~default:""
  in
  (* Reverse navigation (?source_thread=<post_id>): an SSR-visible context notice plus
     data attributes the page JS uses to scroll to the first source message and flash the
     whole group. Everything degrades: with JS off the notice still explains the state and
     the links still work; without source_focus the page is byte-identical to before. *)
  let source_notice, source_data_attrs =
    match source_focus with
    | None -> ("", "")
    | Some (post_id, post_title, highlight_ids) ->
        let thread_href = Components.canonical_thread_path community.slug post_id post_title in
        let short_title =
          if String.length post_title > 60 then String.sub post_title 0 59 ^ "\xe2\x80\xa6" else post_title in
        let notice = Printf.sprintf
          "<div class='cs-source-notice'><span class='cs-source-notice-text'>Viewing the conversation promoted to <b>%s</b></span><span class='cs-source-notice-actions'><a href='%s'>&larr; Back to thread</a><a href='%s'>Jump to latest &darr;</a></span></div>"
          (esc short_title) thread_href channel_url in
        let attrs = match highlight_ids with
          | [] -> ""
          | first :: _ ->
              Printf.sprintf " data-source-anchor-id='%Ld' data-source-highlight-ids='%s'"
                first (Start_thread.highlight_ids_attr highlight_ids) in
        (notice, attrs)
  in
  (* The typing row sits between the scrolling message body and the composer
     (Discord placement): it never scrolls with history and keeps its reserved
     height when empty so the composer doesn't jump. JS fills it by id.
     cs-chat-stage wraps the scroller with a sibling shared-cursor overlay
     covering its visible box; both are empty/inert without JS. *)
  let main =
    Printf.sprintf
      "%s%s<div class='cs-chat-stage'><div id='chat-live-root' class='cs-main-body cs-chat-body' data-channel-id='%d' data-can-start='%s' data-socket-url='%s' data-signed-token='%s'%s>%s</div><div class='cs-cursor-overlay' id='chat-cursor-overlay' aria-hidden='true'></div></div><div class='cs-typing' id='chat-typing' hidden></div>%s"
      head
      source_notice
      channel.id
      (if can_start then "true" else "false")
      (Components.html_escape realtime_socket_url)
      (Components.html_escape realtime_signed_token)
      source_data_attrs
      messages_html
      composer
  in
  (* Presence pane: markup (classes + every #chat-presence-* id chat_live.js
     fills) unchanged; only the outer wrapper is the launch `.aside--chat`
     column instead of the legacy `.cs-aside` grid cell. *)
  let presence_pane =
    "<div class='cs-presence' id='chat-presence'>\
       <div class='ca-label' id='chat-presence-heading'>In this channel</div>\
       <div class='cs-presence-status' id='chat-presence-status'>Connecting&#8230;</div>\
       <ul class='cs-presence-list' id='chat-presence-list'></ul>\
     </div>"
  in
  let aside = Printf.sprintf "<aside class='aside aside--chat'>%s</aside>" presence_pane in
  let title = Printf.sprintf "#%s · %s" channel.name community.name in
  (* The parameterized reverse-navigation view canonicalizes to the clean channel URL so
     crawlers never index per-thread duplicates of the same channel page. *)
  let canonical_link =
    if source_focus = None then ""
    else Printf.sprintf "<link rel='canonical' href='%s'>" channel_url
  in
  let head_extra =
    canonical_link ^
    "<script src='/static/js/phoenix.js' defer></script>\
     <script src='/static/js/chat_live.js' defer></script>"
  in
  (* The complete <main> element, built here so the launch wrapper can never
     interpose a box: .cs-main is a flex column whose head / chat stage /
     typing row / composer MUST stay direct children, and for a private
     community the ph-no-capture replay guard rides on this element itself
     (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    Printf.sprintf "<main class='%s'>%s</main>"
      (if community.visibility = Db.Community_private then "cs-main ph-no-capture" else "cs-main")
      main
  in
  Components.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~head_extra ~aside ~community ~sidebar
    ~page_class:"launch-community-channel" ~title ~main_el ()

(* What the thread page may say about a promoted thread's chat origin, decided by the
   HANDLER from the viewer's read authorization on the source channel's community:
   Ts_visible carries the (slug, name) of the source channel (None if the channel was
   deleted) plus the chronological source rows; Ts_private renders only a neutral
   "promoted from a private conversation" notice — no channel name, authors, content,
   or timestamps ever reach an unauthorized viewer's markup. *)
type thread_source_view =
  | Ts_private
  | Ts_visible of (string * string) option * Db.thread_source_msg list

(* /c/:slug/t/:post_id-:post_slug — the canonical thread view, now on the Cartographic Civic
   launch chrome (Components.launch_community_surface_page: earde.css only, no shell.css).
   Replaces the legacy warm-card post_page for normal threads (post_page stays only as the
   unmappable-post fallback and is left byte-for-byte unchanged). Visible comment/composer UI
   keeps its markup and is re-skinned by the route-scoped CSS; the security-critical
   mod/admin/ban *dialogs* are copied verbatim from post_page (identical routes, CSRF, dialog
   ids and Rule A/B/C logic) so moderation behavior cannot drift — re-skinning those overlays
   would be risky for zero user-facing benefit.
   The optimistic-vote DOM contract (cs-vote = [upvote form, score span, downvote form], buttons
   carrying the exact legacy Tailwind colour classes the shared vote JS toggles — mapped to
   launch accents in earde.css) is preserved exactly. *)
let thread_shell_page ?user ?(noindex=false) ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames
    ~banned_usernames ~(rail_communities : community list) ~(channels : channel list)
    ~(sections : community_section list) ~(community : community)
    ?(thread_source : thread_source_view option)
    ~user_post_votes ~user_comment_votes ~(post : post) ~(comments : comment list) request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in

  (* Launch community sidebar (pass-8/9 grammar): the post's own (parent) section is active.
     Settings gate mirrors the overview: render-time visibility only, handler re-checks. *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ?active_section_slug:post.section_slug
      ~can_manage:(is_current_user_mod || is_admin) ()
  in

  (* Breadcrumb + back-to-section (only when the post actually belongs to a section). *)
  let section_crumb = match post.section_name, post.section_slug with
    | Some sn, Some ss -> Printf.sprintf " / <a href='/c/%s/s/%s'>§ %s</a>" (esc community.slug) (esc ss) (esc sn)
    | _ -> "" in
  let crumb = Printf.sprintf "<div class='fh-crumb'><a href='/c/%s'>/c/%s</a>%s / <b>thread</b></div>"
    (esc community.slug) (esc community.slug) section_crumb in
  let back_link = match post.section_name, post.section_slug with
    | Some sn, Some ss -> Printf.sprintf "<a class='btn sm' href='/c/%s/s/%s'>&larr; %s</a>" (esc community.slug) (esc ss) (esc sn)
    | _ -> "" in

  (* --- post vote column: reuses the proven cs-vote contract from render_forum_row --- *)
  let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_post_votes) in
  let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote = 1 then 0 else 1 in
  let down_action = if current_vote = -1 then 0 else -1 in
  let upvote_html = match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9650;</button></form>" csrf_token post.id up_action up_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500'>&#9650;</a>" in
  let downvote_html =
    if not post.allow_downvotes then ""
    else match current_user with
    | Some _ -> Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9660;</button></form>" csrf_token post.id down_action down_color
    | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2]'>&#9660;</a>" in
  let vote_col = Printf.sprintf "<div class='cs-vote'>%s<span class='cs-vote-score'>%d</span>%s</div>" upvote_html post.score downvote_html in

  (* --- post moderation controls (delete-own / mod-remove / admin-remove / ban), verbatim --- *)
  let is_post_deleted =
    (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_")
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let post_target_is_admin = List.mem post.username admin_usernames in
  let post_action_btn =
    if is_post_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">%s<input type='hidden' name='post_id' value='%d'><button type='submit' class='ct-act ct-act-danger'>&#128465;&#65039; delete</button></form>" csrf_token post.id
        else if is_current_user_mod && not post_target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='ct-act ct-act-mod'>&#128737;&#65039; Mod Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-amber-900'>&#128737;&#65039; Moderator Removal</h3>
                  <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain why this post is being removed (visible to the community)...'
                      class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
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
        else if is_admin && not post_target_is_admin then
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Remove</button>
            <dialog id='mod-modal-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
              <div class='bg-white rounded-2xl overflow-hidden'>
                <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                  <h3 class='text-base font-bold text-red-900'>&#9889; Admin Intervention</h3>
                  <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                </div>
                <form action='/c/%s/posts/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                  %s
                  <label class='flex flex-col gap-1.5'>
                    <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                    <textarea name='reason' required maxlength='255' rows='4'
                      placeholder='Explain the admin intervention reason (visible to the community)...'
                      class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
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
  let ban_post_btn =
    if (is_current_user_mod || is_admin) && not (post_target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username
          && not (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_") ->
          if List.mem post.username banned_usernames then
            "<span class='ct-act ct-act-danger'>&#128683; Banned</span>"
          else if is_current_user_mod then
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='ct-act ct-act-mod'>&#128296; Mod Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-amber-900'>&#128296; Mod Ban</h3>
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
                        class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (esc post.username) post.community_id post.id
          else
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                <div class='bg-white rounded-2xl overflow-hidden'>
                  <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                    <h3 class='text-base font-bold text-red-900'>&#9889; Admin Ban</h3>
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
                        class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (esc post.username) post.community_id post.id
      | _ -> ""
    else ""
  in
  (* Quiet secondary "Report" affordance for logged-in non-authors. Render-time gate is
     visibility only; create_report_handler re-checks login / ban / self-report. *)
  let post_report_btn =
    if is_post_deleted then ""
    else match current_user with
    | Some u when u <> post.username ->
        Printf.sprintf "<a class='ct-act' href='/c/%s/report?type=post&amp;id=%d'>&#9873; report</a>"
          (esc post.community_slug) post.id
    | _ -> "" in
  let post_mod_controls = post_action_btn ^ ban_post_btn ^ post_report_btn in

  (* --- post meta + body --- *)
  let domain_html = match post.url with
    | Some u -> (match Components.extract_domain u with
        | Some d -> Printf.sprintf "<a class='ft-domain' href='%s' target='_blank' rel='noopener'>%s &#8599;</a>" (Components.safe_url u) (esc d)
        | None -> "")
    | None -> "" in
  let meta = Printf.sprintf "<div class='th-meta'>%s<span>by %s</span><span>%s</span><span>%d comments</span></div>"
    domain_html (Components.render_author ~mod_usernames ~admin_usernames post.username)
    (esc (Components.time_ago post.created_at)) post.comment_count in
  (* Post mod/owner actions get their own compact row under the meta — no far-right float. *)
  let post_actions_row =
    if post_mod_controls = "" then "" else Printf.sprintf "<div class='th-actions'>%s</div>" post_mod_controls in
  (* Promoted-conversation block. Real provenance lives in thread_source_messages — this
     is its display, always rendered from the persisted relations regardless of the
     curator's editable introduction (the post body). Renders nothing for a normal post.
     Source messages are display-only structured rows: never forum replies, no vote UI,
     no forum authorship. If the source channel was deleted (FK SET NULL) surviving rows
     still render, just without channel links. Ts_private renders only a neutral notice. *)
  let source_html =
    match thread_source with
    | None -> ""
    | Some Ts_private ->
        "<div class='th-src th-src--private'>\
           <div class='th-src-label'>Promoted conversation</div>\
           <div class='th-src-private-note'>This thread was promoted from a private conversation.</div>\
         </div>"
    | Some (Ts_visible (None, [])) -> ""
    | Some (Ts_visible (channel_opt, msgs)) ->
        let summary = Start_thread.summarize_source msgs in
        let from_html = match channel_opt with
          | Some (cslug, cname) ->
              Printf.sprintf "Promoted from <a href='/c/%s/ch/%s'>#%s</a>"
                (esc community.slug) (esc cslug) (esc cname)
          | None -> "Promoted from chat" in
        let plural n = if n = 1 then "" else "s" in
        let meta_bits =
          (if summary.Start_thread.ss_date_range = "" then []
           else [ esc summary.Start_thread.ss_date_range ])
          @ [ Printf.sprintf "%d message%s" summary.Start_thread.ss_available (plural summary.Start_thread.ss_available)
            ; Printf.sprintf "%d participant%s" summary.Start_thread.ss_participants (plural summary.Start_thread.ss_participants) ]
          @ (if summary.Start_thread.ss_unavailable = 0 then []
             else [ Printf.sprintf "%d unavailable" summary.Start_thread.ss_unavailable ]) in
        let meta_html = String.concat " &middot; " meta_bits in
        let row (m : Db.thread_source_msg) =
          if m.sm_deleted then
            "<div class='th-src-msg th-src-msg--gone'><span class='th-src-unavailable'>[message unavailable]</span></div>"
          else
            let name = if String.trim m.sm_author = "" then "[deleted]" else m.sm_author in
            let seed_badge = if m.sm_is_seed then "<span class='th-src-seed'>seed</span>" else "" in
            let chat_link = match channel_opt with
              | Some (cslug, _) ->
                  Printf.sprintf "<a class='th-src-jump' href='/c/%s/ch/%s?source_thread=%d#msg-%Ld'>view in chat</a>"
                    (esc community.slug) (esc cslug) post.id m.sm_id
              | None -> "" in
            Printf.sprintf
              "<div class='th-src-msg'><div class='th-src-msg-meta'><span class='th-src-author'>%s</span>%s<span class='th-src-time'>%s</span>%s</div><div class='th-src-text'>%s</div></div>"
              (esc name) seed_badge (esc (Start_thread.minute_of_ts m.sm_created_at)) chat_link (esc m.sm_content) in
        let rows_html = String.concat "" (List.map row msgs) in
        let view_original = match channel_opt with
          | Some (cslug, _) ->
              Printf.sprintf
                "<div class='th-src-foot'><a href='/c/%s/ch/%s?source_thread=%d'>View original conversation &rarr;</a></div>"
                (esc community.slug) (esc cslug) post.id
          | None -> "" in
        Printf.sprintf
          "<div class='th-src'>\
             <div class='th-src-label'>Promoted conversation</div>\
             <div class='th-src-head'>%s</div>\
             <div class='th-src-meta'>%s</div>\
             <div class='th-src-msgs'>%s</div>\
             %s\
           </div>"
          from_html meta_html rows_html view_original in
  let body_html =
    let img = match post.image_url with
      | Some i when i <> "" -> Printf.sprintf "<div class='th-img'><img src='%s' alt='Post image'></div>" (esc i)
      | _ -> "" in
    let txt = match post.content with
      | Some c when String.trim c <> "" -> Printf.sprintf "<div class='th-body'>%s</div>" (esc c)
      | _ -> "" in
    let link = match post.url with
      | Some u -> Printf.sprintf "<div class='th-linkrow'><a class='btn sm' href='%s' target='_blank' rel='noopener'>Open link &#8599;</a></div>" (Components.safe_url u)
      | None -> "" in
    img ^ txt ^ link in

  (* --- comment composer (shell-styled) --- *)
  let composer =
    if is_member then
      Printf.sprintf "<form class='ct-composer' action='/comments' method='POST'>%s<input type='hidden' name='post_id' value='%d'><textarea name='content' required rows='3' placeholder='Add to the thread&#8230;'></textarea><div class='ct-composer-actions'><button type='submit' class='btn sm primary'>Reply</button></div></form>" csrf_token post.id
    else match current_user with
      (* Private community: viewer is an authorized non-member (mod/admin); no self-join button. *)
      | Some _ when community.visibility = Db.Community_private ->
          Printf.sprintf "<div class='ct-join'><span>Only members of <a href='/c/%s'>/c/%s</a> can reply.</span></div>"
            (esc community.slug) (esc community.slug)
      | Some _ ->
          Printf.sprintf "<div class='ct-join'><span>You must be a member of <a href='/c/%s'>/c/%s</a> to reply.</span><form action='/join' method='POST' class='inline'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn sm primary'>Join /c/%s</button></form></div>"
            (esc community.slug) (esc community.slug) csrf_token post.community_id
            (Components.canonical_thread_path post.community_slug post.id post.title) (esc community.slug)
      | None ->
          "<div class='ct-join'><span><a href='/login'>Log in</a> to join the discussion.</span></div>" in

  (* --- comments, shell-styled. Mod/admin/ban dialogs copied verbatim from post_page. --- *)
  let rec render_comment_tree all_comments parent_id depth =
    let children = List.filter (fun (c : comment) -> c.parent_id = parent_id) all_comments in
    if children = [] then ""
    else String.concat "\n" (List.map (fun (c : comment) ->
      let nested = render_comment_tree all_comments (Some c.id) (depth + 1) in
      let cvote = Option.value ~default:0 (List.assoc_opt c.id user_comment_votes) in
      let up_color = if cvote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
      let down_color = if cvote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
      let up_action = if cvote = 1 then 0 else 1 in
      let down_action = if cvote = -1 then 0 else -1 in
      let upvote_html = match current_user with
        | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9650;</button></form>" csrf_token c.id up_action up_color
        | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500'>&#9650;</a>" in
      let downvote_html =
        if not post.allow_downvotes then ""
        else match current_user with
        | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s'>&#9660;</button></form>" csrf_token c.id down_action down_color
        | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2]'>&#9660;</a>" in
      let cvote_pill = Printf.sprintf "<div class='cs-vote'>%s<span class='cs-vote-score'>%d</span>%s</div>" upvote_html c.score downvote_html in

      let reply_button = if is_member then
          Printf.sprintf "<button type='button' class='ct-act' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\">&#8624; reply</button>" c.id
        else "" in
      let reply_form = if is_member then
          Printf.sprintf "<form id='reply-form-%d' class='ct-composer ct-reply hidden' action='/comments' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='parent_id' value='%d'><textarea name='content' required rows='2' placeholder='Write a reply&#8230;'></textarea><div class='ct-composer-actions'><button type='button' class='btn sm' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\">Cancel</button><button type='submit' class='btn sm primary'>Reply</button></div></form>" c.id csrf_token post.id c.id c.id
        else "" in

      let is_comment_deleted =
        Components.is_deleted_user c.username
        || c.content = "[deleted]"
        || c.content = "[removed by admin]"
        || c.content = "[removed by moderator]" in
      let comment_target_is_admin = List.mem c.username admin_usernames in
      let delete_comment_btn =
        if is_comment_deleted then ""
        else match current_user with
        | None -> ""
        | Some u ->
            if u = c.username then
              Printf.sprintf "<form action='/delete-comment' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this comment? This action cannot be undone.')\">%s<input type='hidden' name='comment_id' value='%d'><button type='submit' class='ct-act ct-act-danger'>&#128465;&#65039;</button></form>" csrf_token c.id
            else if is_current_user_mod && not comment_target_is_admin then
              Printf.sprintf "
                <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='ct-act ct-act-mod'>&#128737;&#65039; Mod Remove</button>
                <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                  <div class='bg-white rounded-2xl overflow-hidden'>
                    <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                      <h3 class='text-base font-bold text-amber-900'>&#128737;&#65039; Moderator Removal</h3>
                      <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                    </div>
                    <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                      %s
                      <label class='flex flex-col gap-1.5'>
                        <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                        <textarea name='reason' required maxlength='255' rows='4'
                          placeholder='Explain why this comment is being removed (visible to the community)...'
                          class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                      </label>
                      <div class='flex justify-end gap-2 pt-1'>
                        <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                          class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                        <button type='submit'
                          class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                      </div>
                    </form>
                  </div>
                </dialog>"
                c.id c.id post.community_slug c.id csrf_token c.id
            else if is_admin && not comment_target_is_admin then
              Printf.sprintf "
                <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Remove</button>
                <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                  <div class='bg-white rounded-2xl overflow-hidden'>
                    <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                      <h3 class='text-base font-bold text-red-900'>&#9889; Admin Intervention</h3>
                      <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                    </div>
                    <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                      %s
                      <label class='flex flex-col gap-1.5'>
                        <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                        <textarea name='reason' required maxlength='255' rows='4'
                          placeholder='Explain the admin intervention reason (visible to the community)...'
                          class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                      </label>
                      <div class='flex justify-end gap-2 pt-1'>
                        <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                          class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                        <button type='submit'
                          class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                      </div>
                    </form>
                  </div>
                </dialog>"
                c.id c.id post.community_slug c.id csrf_token c.id
            else "" in
      let ban_comment_btn =
        if (is_current_user_mod || is_admin) && not (comment_target_is_admin && not is_admin) then
          match current_user with
          | Some u when u <> c.username && not (Components.is_deleted_user c.username) ->
              if List.mem c.username banned_usernames then
                "<span class='ct-act ct-act-danger'>&#128683; Banned</span>"
              else if is_current_user_mod then
                Printf.sprintf "
                  <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='ct-act ct-act-mod'>&#128296; Mod Ban</button>
                  <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-amber-900'>&#128296; Mod Ban</h3>
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
                            class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id csrf_token (esc c.username) post.community_id c.id
              else
                Printf.sprintf "
                  <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='ct-act ct-act-admin'>&#9889; Admin Ban</button>
                  <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-red-900'>&#9889; Admin Ban</h3>
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
                            class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id csrf_token (esc c.username) post.community_id c.id
          | _ -> ""
        else "" in

      (* Quiet secondary "Report" affordance on each comment for logged-in non-authors.
         Visibility-only gate; the POST handler re-checks login / ban / self-report. *)
      let comment_report_btn =
        if is_comment_deleted then ""
        else match current_user with
        | Some u when u <> c.username ->
            Printf.sprintf "<a class='ct-act' href='/c/%s/report?type=comment&amp;id=%d'>&#9873; report</a>"
              (esc post.community_slug) c.id
        | _ -> "" in
      let op_badge = if c.username = post.username then "<span class='role op'>OP</span>" else "" in
      let toggle_btn = Printf.sprintf "<button type='button' class='ct-collapse' onclick='toggleComment(%d, this)'>[-]</button>" c.id in
      (* Cap the indent after a few levels so deep chains don't staircase off-screen; the thread
         structure is unchanged (parent_id recursion), only the left-rail indent flattens. *)
      let children_cls = if depth >= 3 then "cmt-children cmt-flat" else "cmt-children" in
      let children_div = if nested = "" then "" else Printf.sprintf "<div id='comment-children-%d' class='%s'>%s</div>" c.id children_cls nested in
      let reply_cls = if depth > 0 then " reply" else "" in
      (* children_div is a SIBLING of .cmt (not nested in .cbody) so each level indents only by the
         guide rail, not by the parent's vote column — keeps deep threads from staircasing.
         Collapse still works: toggleComment finds comment-children-<id> by id wherever it sits. *)
      Printf.sprintf "
      <div class='cmt%s'>
          %s
          <div class='cbody'>
              <div class='cmeta'>%s %s %s <span class='ctime'>%s</span></div>
              <div id='comment-content-%d'>
                  <div class='ctext'>%s</div>
                  <div class='cactions'>%s%s%s%s</div>
                  %s
              </div>
          </div>
      </div>%s"
      reply_cls
      cvote_pill
      toggle_btn (Components.render_author ~mod_usernames ~admin_usernames c.username) op_badge
      (esc (Components.time_ago c.created_at))
      c.id
      (esc c.content)
      reply_button delete_comment_btn ban_comment_btn comment_report_btn
      reply_form
      children_div
    ) children) in
  let comments_html =
    if comments = [] then "<div class='cs-empty'>No comments yet &mdash; start the discussion.</div>"
    else render_comment_tree comments None 0 in

  (* --- main pane --- *)
  (* Local tab row — only the real "Thread" view. Future Source-chat/Backlinks slots are
     intentionally omitted until those features exist (no unavailable tabs, no fake counts). *)
  let th_tabs =
    "<div class='th-tabs'><span class='th-tab active'>Thread</span></div>" in
  let topbar = Printf.sprintf "<div class='th-topbar'><div class='th-topbar-row'>%s%s</div>%s</div>"
    crumb
    (if back_link = "" then "" else Printf.sprintf "<div class='th-back'>%s</div>" back_link)
    th_tabs in
  let post_block = Printf.sprintf "
    <article class='th-main'>
        <div class='th-head'>
            %s
            <div class='th-headmain'>
                <h1 class='th-title'>%s</h1>
                %s
                %s
                %s
                %s
            </div>
        </div>
    </article>"
    vote_col (esc post.title) meta post_actions_row source_html body_html in
  let discussion = Printf.sprintf "
    <section class='th-discussion'>
        <div class='th-comments-label'>// %d comments</div>
        %s
        %s
    </section>"
    post.comment_count composer comments_html in
  (* .thread-shell-main is the readable column AND the scroll pane: it sits straight inside cs-main
     (no cs-main-body) and shell.css makes it flex:1 + overflow-y:auto, so the thread scrolls inside
     the viewport-locked app shell (same containment as feed/section/chat) — the topbar and the
     rail/sidebar/right rail stay fixed; only this pane scrolls. *)
  let main = Printf.sprintf "<div class='thread-shell-main'>%s%s%s</div>" topbar post_block discussion in

  (* --- right rail: real data only --- *)
  let section_row = match post.section_name, post.section_slug with
    | Some sn, Some ss -> Printf.sprintf "<tr><td>Section</td><td class='num'><a href='/c/%s/s/%s'>%s</a></td></tr>" (esc community.slug) (esc ss) (esc sn)
    | _ -> "<tr><td>Section</td><td class='num'>&mdash;</td></tr>" in
  let back_cta = match post.section_name, post.section_slug with
    | Some sn, Some ss -> Printf.sprintf "<div class='ca-cta'><a class='btn sm block' href='/c/%s/s/%s'>&larr; Back to %s</a></div>" (esc community.slug) (esc ss) (esc sn)
    | _ -> "" in
  let stats_block = Printf.sprintf "
    <div class='ca-block'>
        <div class='ca-label'>Thread &middot; indexed</div>
        <table class='ca-grid'><tbody>
            <tr><td>Community</td><td class='num'><a href='/c/%s'>/c/%s</a></td></tr>
            %s
            <tr><td>Created</td><td class='num'>%s</td></tr>
            <tr><td>Replies</td><td class='num'>%d</td></tr>
            <tr><td>Score</td><td class='num'>%d</td></tr>
        </tbody></table>
        %s
    </div>"
    (esc community.slug) (esc community.slug) section_row
    (esc (Components.time_ago post.created_at)) post.comment_count post.score back_cta in
  (* Participants: deduped, real authors from the loaded post + comments; tombstoned users skipped. *)
  let participants_block =
    let names = post.username :: List.map (fun (c : comment) -> c.username) comments in
    let seen = Hashtbl.create 16 in
    let uniq = List.filter (fun u ->
      if Components.is_deleted_user u || Hashtbl.mem seen u then false
      else (Hashtbl.add seen u (); true)) names in
    if uniq = [] then ""
    else
      let rows = String.concat "" (List.map (fun u ->
        let badge = if List.mem u mod_usernames then "<span class='role mod'>MOD</span>" else "" in
        Printf.sprintf "<div class='member'><a href='/u/%s'>%s</a>%s</div>" (esc u) (esc u) badge) uniq) in
      Printf.sprintf "<div class='ca-block'><div class='ca-label'>Participants</div>%s</div>" rows in
  (* Source participants: authors of the promoted chat conversation. A separately labeled
     group — deliberately NOT deduplicated against forum Participants above, because the
     two lists communicate different roles (spoke in the source chat vs. posted in the
     thread). Tombstoned/unavailable identities are skipped: there is no safe generic
     identity chip in the current UI, and the block's "N unavailable" note already covers
     the gap. Only rendered for viewers authorized to see the source (Ts_visible). *)
  let source_participants_block =
    match thread_source with
    | Some (Ts_visible (_, msgs)) ->
        let names =
          List.fold_left (fun acc (m : Db.thread_source_msg) ->
            let a = String.trim m.sm_author in
            if m.sm_deleted || a = "" || List.mem a acc then acc else acc @ [a]) [] msgs in
        if names = [] then ""
        else
          let rows = String.concat "" (List.map (fun u ->
            Printf.sprintf "<div class='member'><a href='/u/%s'>%s</a></div>" (esc u) (esc u)) names) in
          Printf.sprintf "<div class='ca-block'><div class='ca-label'>Source participants</div>%s</div>" rows
    | _ -> "" in
  let right_pane = stats_block ^ participants_block ^ source_participants_block in

  (* --- head: canonical + meta description + page-scoped collapse script --- *)
  let canonical = Components.canonical_thread_path post.community_slug post.id post.title in
  let excerpt =
    match post.content with
    | Some c when String.trim c <> "" ->
        let c = String.trim c in
        if String.length c > 155 then String.sub c 0 155 ^ "\xe2\x80\xa6" else c
    | _ -> (match post.url with Some u -> u | None -> post.title) in
  let head_extra = Printf.sprintf
    "<link rel='canonical' href='%s'><meta name='description' content='%s'><script src='/static/js/thread.js' defer></script>"
    (esc canonical) (esc excerpt) in

  (* The complete <main> element, built here so the launch wrapper can never interpose a
     box: .cs-main is a flex column whose only child (.thread-shell-main) owns the scroll,
     and for a private community the ph-no-capture replay guard rides on this element
     itself (never a wrapper div — see the chat-layout regression). *)
  let main_el =
    Printf.sprintf "<main class='%s'>%s</main>"
      (if community.visibility = Db.Community_private then "cs-main ph-no-capture" else "cs-main")
      main
  in
  let aside = Printf.sprintf "<aside class='aside'>%s</aside>" right_pane in
  Components.launch_community_surface_page ?user ~noindex ~request ~rail_communities
    ~head_extra ~aside ~community ~sidebar ~page_class:"launch-community-thread"
    ~title:post.title ~main_el ()

(* /c/:slug — the public community home / overview / entry page. NOT the persistent shell
   (that is Components.community_shell, used by section feeds). Flat cool-grey markup scoped
   under .community-home; every datum here is real — no fake member/online/message counts and
   no created_at, because Db.community carries neither. SSR-only: every link/form works with JS
   off. Public presentation only — management (downvotes, mods, sections, bans) lives in
   /c/:slug/settings; the only management affordance here is the gated "Edit community" link. *)
(* [connected_projects]: see community_page — the same pre-rendered fragment, supplied by the
   same /c/:slug route after its existing authorization, and "" when there is nothing to show. *)
(* /c/:slug (structured) — the community overview, now on the Cartographic
   Civic launch chrome (Components.launch_community_page: earde.css only, no
   shell.css/community-home.css/Tailwind). Every form, destination, and
   permission gate is the pre-launch contract reskinned: the /join and /leave
   POSTs keep their exact fields, the settings and review links keep their
   existing mod/top-mod gates, and the pre-rendered ccp-* connected-projects
   fragment is spliced in verbatim (its markup is pinned by the fragment
   suites) and restyled purely by the route-scoped CSS. *)
let community_overview_page ?user ?(noindex=false) ?(connected_projects="") ~is_member ~is_current_user_mod ~is_current_user_top_mod
    ~mod_usernames ~orphaned ~(rail_communities : community list)
    ~(channels : channel list) ~(recent_posts : post list)
    (community : community) (section_stats : (community_section * int * string option) list) request =
  let csrf_token = Dream.csrf_tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let esc = Components.html_escape in
  let (orphaned_count, _) = orphaned in
  let slug = esc community.slug in

  (* Real stats only. sections/threads/channels are derivable from data already loaded; the
     prototype's "members"/"since" have no backing column/query, so they are omitted. *)
  let section_count = List.length section_stats + (if orphaned_count > 0 then 1 else 0) in
  let thread_count =
    List.fold_left (fun acc (_, c, _) -> acc + c) 0 section_stats + orphaned_count in
  let channel_count = List.length channels in
  let plural n = if n = 1 then "" else "s" in

  (* Community face: the avatar image when one is set and passes the gate
     (safe_img_src accepts the local /static/uploads/ path), else the launch
     rail's capitalized 2-letter slug glyph on the deterministic palette
     tone — the same fallback every launch tile uses, so the crest, sidebar
     head, and rail tile all agree. *)
  let tile_glyph =
    String.capitalize_ascii
      (if String.length community.slug >= 2 then String.sub community.slug 0 2
       else if community.slug = "" then "?" else community.slug)
  in
  let tile_color = Components.launch_tile_color community.slug in
  let face size_cls =
    match community.avatar_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" ->
             Printf.sprintf "<span class='avatar %s' style='background:%s'>%s</span>"
               size_cls tile_color (esc tile_glyph)
         | src ->
             Printf.sprintf "<span class='avatar %s'><img class='launch-avatar-img' src='%s' alt=''></span>"
               size_cls src)
    | _ ->
        Printf.sprintf "<span class='avatar %s' style='background:%s'>%s</span>"
          size_cls tile_color (esc tile_glyph)
  in

  (* Primary CTA target = the community's General section feed (every community has one after
     the default-structure merge). Prefer the canonical "general" slug, fall back to the first
     section — so a renamed/reordered General still resolves and the link is never dead. *)
  let target_section =
    match List.find_opt (fun ((s : community_section), _, _) -> s.slug = "general") section_stats with
    | Some s -> Some s
    | None -> (match section_stats with s :: _ -> Some s | [] -> None)
  in

  (* The primary CTA opens chat. Prefer the canonical "general" channel, fall back to the
     first channel (mirrors target_section) so the link is never dead; if a community somehow
     has no channels we fall back to the section feed below. *)
  let target_channel =
    match List.find_opt (fun (c : channel) -> c.slug = "general") channels with
    | Some c -> Some c
    | None -> (match channels with c :: _ -> Some c | [] -> None)
  in

  (* Three-state CTA, all wired to existing routes — no chat route invented:
     - anon: send to the existing /login entry point.
     - logged-in non-member: POST the existing /join route with redirect_to the chat/feed.
     - member: a plain link straight into chat. Fields and semantics are the
       pre-launch contract byte-for-byte; only the button classes changed. *)
  let primary_cta =
    match user with
    | None ->
        "<a class='btn btn--primary' href='/login'>Log in to join &rarr;</a>"
    | Some _ ->
        (match target_channel with
        | Some (c : channel) ->
            let chat_url = Printf.sprintf "/c/%s/ch/%s" slug (esc c.slug) in
            (* Private community: anyone seeing this overview is authorized to read it, so link
               straight in — never show a self-join button (Slice C). *)
            if is_member || community.visibility = Db.Community_private then
              Printf.sprintf "<a class='btn btn--primary' href='%s'>Open #%s &rarr;</a>" chat_url (esc c.slug)
            else
              Printf.sprintf "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn btn--primary btn--block'>Join &amp; open #%s &rarr;</button></form>"
                csrf_token community.id chat_url (esc c.slug)
        | None ->
            (* No channels (shouldn't happen post default-structure) — fall back to a section feed. *)
            (match target_section with
             | Some ((s : community_section), _, _) ->
                 let feed_url = Printf.sprintf "/c/%s/s/%s" slug (esc s.slug) in
                 if is_member || community.visibility = Db.Community_private then
                   Printf.sprintf "<a class='btn btn--primary' href='%s'>Open %s &rarr;</a>"
                     feed_url (esc s.name)
                 else
                   Printf.sprintf "<form action='/join' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='%s'><button type='submit' class='btn btn--primary btn--block'>Join &amp; open %s &rarr;</button></form>"
                     csrf_token community.id feed_url (esc s.name)
             | None when orphaned_count > 0 ->
                 Printf.sprintf "<a class='btn btn--primary' href='/c/%s/s/uncategorized'>Browse threads &rarr;</a>"
                   slug
             | None -> ""))
  in

  (* The primary CTA already handles joining for non-members, so the secondary slot only needs
     the Leave action for existing members. Same POST /leave contract as before. *)
  let membership_btn =
    match user with
    | Some _ when is_member ->
        Printf.sprintf "<form action='/leave' method='POST'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='btn btn--secondary btn--block launch-leave'>Leave</button></form>" csrf_token community.id slug
    | _ -> ""
  in

  let settings_btn =
    if is_current_user_mod || is_admin then
      Printf.sprintf "<a class='btn btn--secondary' href='/c/%s/settings'>&#9881; Settings</a>" slug
    else ""
  in

  (* --- community sidebar: real navigation only. Channels and sections keep
     their real destinations (those pages stay legacy in this pass);
     Moderation log is intentionally public; Settings and Home requests
     render only under the exact gates their handlers enforce. *)
  let side_head =
    Printf.sprintf
      "<a class='sidebar__head' href='/c/%s'>%s<span class='launch-side-id'><span class='sidebar__name'>%s</span><span class='sidebar__slug'>/c/%s</span></span></a>"
      slug (face "avatar--32") (esc community.name) slug
  in
  let nav_overview =
    Printf.sprintf
      "<a class='navitem navitem--pad navitem--active' href='/c/%s'><span class='navitem__sigil navitem__sigil--box'>&#8962;</span>Overview</a>"
      slug
  in
  (* Visibility state, stated factually where the viewer already is. Public
     is the default and carries no marker. *)
  let vis_note =
    if community.visibility = Db.Community_private then
      "<div class='launch-side-vis'>private community</div>"
    else ""
  in
  let nav_live =
    if channels = [] then ""
    else
      "<p class='kicker sidebar__group'>Live</p>"
      ^ String.concat "" (List.map (fun (c : channel) ->
          Printf.sprintf
            "<a class='navitem' href='/c/%s/ch/%s'><span class='navitem__sigil navitem__sigil--live'>#</span>%s</a>"
            slug (esc c.slug) (esc c.slug))
          channels)
  in
  let nav_sections_items =
    List.map (fun ((s : community_section), post_count, _) ->
        Printf.sprintf
          "<a class='navitem' href='/c/%s/s/%s'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>%s<span class='navitem__trail'>%d</span></a>"
          slug (esc s.slug) (esc s.name) post_count)
      section_stats
    @ (if orphaned_count > 0 then
         [ Printf.sprintf
             "<a class='navitem' href='/c/%s/s/uncategorized'><span class='navitem__sigil navitem__sigil--live'>&sect;</span>Uncategorized<span class='navitem__trail'>%d</span></a>"
             slug orphaned_count ]
       else [])
  in
  let nav_knowledge =
    if nav_sections_items = [] then ""
    else "<p class='kicker sidebar__group'>Knowledge</p>" ^ String.concat "" nav_sections_items
  in
  let nav_network =
    "<p class='kicker sidebar__group'>Network</p>"
    ^ Printf.sprintf
        "<a class='navitem navitem--pad' href='/c/%s/modlog'><span class='navitem__sigil navitem__sigil--box'>&#9776;</span>Moderation log</a>"
        slug
    ^ (if is_current_user_top_mod || is_admin then
         (* Same top_mod-or-admin gate the review-queue handler and the
            settings nav apply to /c/:slug/project-home-requests. *)
         Printf.sprintf
           "<a class='navitem navitem--pad' href='/c/%s/project-home-requests'><span class='navitem__sigil navitem__sigil--box navitem__sigil--project'>&#9672;</span>Home requests</a>"
           slug
       else "")
    ^ (if is_current_user_mod || is_admin then
         Printf.sprintf
           "<a class='navitem navitem--pad' href='/c/%s/settings'><span class='navitem__sigil navitem__sigil--box'>&#9881;</span>Settings</a>"
           slug
       else "")
  in
  let sidebar =
    Printf.sprintf
      "<aside class='sidebar' aria-label='%s community'>%s<div class='sidebar__body'>%s%s%s%s%s</div></aside>"
      (esc community.name) side_head nav_overview vis_note nav_live nav_knowledge nav_network
  in

  (* --- header band. Badges are facts the record already states: the network
     marker and, for a private community its viewers are inside of, the
     restrained visibility badge. Public is unmarked. *)
  let badges =
    (if community.is_network_community then
       " <span class='badge badge--network badge--lg'>&#9672; Network community</span>"
     else "")
    ^ (if community.visibility = Db.Community_private then
         " <span class='badge badge--plain badge--lg'>Private</span>"
       else "")
  in
  let chead =
    Printf.sprintf
      "<div class='chead'><div class='chead__row'>%s<div class='launch-chead-id'>\
       <div class='titleline'><h1 class='chead__title'>%s</h1><span class='chead__slug'>/c/%s</span>%s</div>\
       <p class='chead__desc'>%s</p>\
       <p class='chead__stats'>%d section%s &middot; %d thread%s &middot; %d channel%s</p>\
       </div><div class='chead__actions'>%s%s%s</div></div></div>"
      (face "avatar--62") (esc community.name) slug badges
      (esc (Option.value ~default:"No description." community.description))
      section_count (plural section_count)
      thread_count (plural thread_count)
      channel_count (plural channel_count)
      primary_cta membership_btn settings_btn
  in

  (* --- knowledge sections: flat panel rows over real per-section counts. --- *)
  let render_section ((s : community_section), post_count, _last_activity) =
    Printf.sprintf
      "<a class='project-row launch-secrow' href='/c/%s/s/%s'><span class='launch-sec-sigil'>&sect;</span><span class='launch-sec-main'><span class='launch-sec-name'>%s</span>%s</span><span class='launch-sec-count'>%d</span></a>"
      slug (esc s.slug) (esc s.name)
      (match s.description with
       | Some d when d <> "" -> Printf.sprintf "<span class='launch-sec-desc'>%s</span>" (esc d)
       | _ -> "")
      post_count
  in
  let uncategorized_row =
    if orphaned_count > 0 then
      Printf.sprintf
        "<a class='project-row launch-secrow' href='/c/%s/s/uncategorized'><span class='launch-sec-sigil'>&sect;</span><span class='launch-sec-main'><span class='launch-sec-name'>Uncategorized</span><span class='launch-sec-desc'>Posts from deleted sections</span></span><span class='launch-sec-count'>%d</span></a>"
        slug orphaned_count
    else ""
  in
  let sections_inner = String.concat "" (List.map render_section section_stats) ^ uncategorized_row in
  let sections_panel =
    Printf.sprintf
      "<section class='panel'><div class='section-head'><span class='kicker'>Knowledge sections</span><span class='count-pill'>%d</span></div>%s</section>"
      section_count
      (if sections_inner = "" then "<div class='empty--inline'>No sections yet.</div>" else sections_inner)
  in

  (* --- recent durable knowledge: real newest posts, linking to the real /p/:id route. --- *)
  let recent_panel =
    if recent_posts = [] then ""
    else
      let rows = String.concat "" (List.map (fun (p : post) ->
        Printf.sprintf
          "<a class='project-row launch-postrow' href='/p/%d'>%s<span class='launch-post-title'>%s</span><span class='launch-post-foot'>&#9650; %d &middot; by %s &middot; %s &middot; &#128172; %d</span></a>"
          p.id
          (match p.section_name with
           | Some n when n <> "" -> Printf.sprintf "<span class='launch-post-meta'><span class='launch-post-sec'>&sect; %s</span></span>" (esc n)
           | _ -> "")
          (esc p.title) p.score (esc p.username) (Components.time_ago p.created_at) p.comment_count)
        recent_posts) in
      Printf.sprintf
        "<section class='panel'><div class='section-head'><span class='kicker'>Recent durable knowledge</span></div>%s</section>"
        rows
  in

  (* --- live channels: real data only, each row links into the SSR chat channel. No fake
     online/talking counts. --- *)
  let channels_panel =
    if channels = [] then ""
    else
      let rows = String.concat "" (List.map (fun (c : channel) ->
        Printf.sprintf
          "<a class='project-row launch-chanrow' href='/c/%s/ch/%s'><span class='launch-chan-line'><span class='launch-chan-hash'>#</span> %s</span>%s</a>"
          slug (esc c.slug) (esc c.slug)
          (match c.topic with
           | Some t when t <> "" -> Printf.sprintf "<span class='launch-chan-topic'>%s</span>" (esc t)
           | _ -> ""))
        channels) in
      Printf.sprintf
        "<section class='panel'><div class='section-head'><span class='kicker'>Live channels</span></div>%s</section>"
        rows
  in

  (* --- moderators. The page knows usernames only (no per-mod roles here), so
     rows carry no role badges — nothing is invented. Moderator management
     stays behind the gated Settings surface. *)
  let mods_inner =
    if mod_usernames = [] then "<div class='empty--inline'>No moderators yet.</div>"
    else
      Printf.sprintf "<div class='launch-mods'>%s</div>"
        (String.concat "" (List.map (fun u ->
          let initial =
            if String.length u > 0 then esc (String.sub (String.uppercase_ascii u) 0 1) else "?" in
          Printf.sprintf
            "<a class='launch-modrow' href='/u/%s'><span class='avatar avatar--22 avatar--mod'>%s</span><span class='launch-mod-name'>u/%s</span></a>"
            (esc u) initial (esc u)) mod_usernames))
  in
  let mods_panel =
    Printf.sprintf
      "<section class='panel'><div class='section-head'><span class='kicker'>Moderators</span></div>%s</section>"
      mods_inner
  in

  let rules_panel = match community.rules with
    | Some r when r <> "" ->
        Printf.sprintf
          "<section class='panel'><div class='section-head'><span class='kicker'>Rules</span></div><div class='panel__body launch-rules'>%s</div></section>"
          (esc r)
    | _ -> ""
  in

  (* Public moderation log card. /c/:slug/modlog is intentionally public (no permission check in
     modlog_handler) so the community's moderation history is transparent; this is a plain link —
     no embedded log, no new query. *)
  let modlog_panel =
    Printf.sprintf
      "<section class='panel'><div class='section-head'><span class='kicker'>Moderation log</span></div><div class='panel__body'><p class='launch-modlog-desc'>A public record of moderation actions in this community, kept open for transparency.</p><a class='btn--link mono launch-modlog-link' href='/c/%s/modlog'>View moderation log &rarr;</a></div></section>"
      slug
  in

  (* Optional banner — rendered only when a safe banner_url is present; absent => no block at
     all, so banner-less communities read exactly like the reference (no gradient placeholder). *)
  let banner_html =
    match community.banner_url with
    | Some url when String.trim url <> "" ->
        (match Components.safe_img_src url with
         | "#" -> ""
         | src -> Printf.sprintf "<div class='launch-banner'><img src='%s' class='launch-banner-img' alt=''></div>" src)
    | _ -> ""
  in

  (* The overview's right column is page content inside the main scroller
     (two-col), not the shell's `.aside` pane — exactly the reference's
     anatomy. The pre-rendered ccp-* fragment leads the left column and is
     spliced verbatim. *)
  let content =
    Printf.sprintf
      "<div class='scroll'>%s%s<div class='container launch-overview-body'><div class='two-col'><div class='stack'>%s%s%s</div><div class='stack stack--sm'>%s%s%s%s</div></div></div></div>"
      banner_html chead
      connected_projects sections_panel recent_panel
      channels_panel mods_panel rules_panel modlog_panel
  in
  Components.launch_community_page ?user ~noindex ~request ~rail_communities
    ~community ~sidebar ~page_class:"launch-community-overview" ~title:community.name ~content ()

(* [connected_projects] is the pre-rendered "Connected projects" management fragment
   (Project_home_removal_pages.community_side_management_section) supplied by the settings
   route, or "" when the viewer is not on the top-mod/admin surface — the route never loads
   the read model for anyone else, and this page never loads it at all. An empty fragment
   also removes the panel from the navigation, so no ordinary moderator can reach an empty
   management surface by typing ?panel=projects. *)
let community_settings_page ?user ?(connected_projects="") ?(rail_communities=[]) ~is_admin ~is_top_mod ~open_reports_count ~(community : community) ~(mods : user list) ~(banned_users : user list) ~(members : user list) ~(sections : community_section list) ~(channels : Db.channel list) request =
  let csrf_token = Dream.csrf_tag request in
  let esc = Components.html_escape in
  let slug = esc community.slug in
  (* Active (non-archived) channel count — used both in the status strip and to gate the
     archive control in the UI (the server enforces the same guards regardless). *)
  let active_channels = List.filter (fun (c : Db.channel) -> not c.is_archived) channels in
  let active_channel_count = List.length active_channels in

  (* The settings surface is a control panel: a left nav of panels, one panel rendered
     at a time. The selected panel comes from ?panel=…; unknown or missing values fall
     back to visibility. Pure server-side rendering — each nav link is a normal GET back
     to this same route with a different query value. *)
  (* The connected-projects panel exists only when the route supplied its fragment, which it
     does only for the top-mod/admin surface. Anyone else asking for ?panel=projects falls
     back to visibility exactly like any unknown value. *)
  let has_connected_projects = connected_projects <> "" in
  let panel =
    match Dream.query request "panel" with
    | Some "projects" when has_connected_projects -> "projects"
    | Some ("profile" | "channels" | "members" | "moderation" | "bans" as p) -> p
    | _ -> "visibility"
  in

  let is_private = community.visibility = Db.Community_private in
  let can_edit_vis = is_top_mod || is_admin in

  (* Network-community lifecycle state. A provisioned setup draft is not
     configured through the legacy settings controls: its identity,
     visibility, indexing, and discovery are all decided at once by the
     canonical publication flow, and the matching legacy POST routes now
     refuse a network community outright. Rendering controls that the server
     would reject would be a lie, so they are replaced by a pointer to the
     canonical surface. *)
  let is_network_draft =
    community.is_network_community
    && community.onboarding_state = Db.Community_draft
  in
  (* The scoped network-slug grammar the database enforces on every network
     row. Defensive: a slug outside it never becomes a setup link. *)
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
  in
  (* The setup surface independently reauthorizes (current top_mod of this
     community, or a durable users.is_admin holder), so this only decides
     whether the affordance is worth showing on a surface that already knows
     the answer for the common cases. *)
  let can_complete_setup =
    is_network_draft && can_edit_vis && canonical_network_slug community.slug
  in
  let setup_pointer_note =
    if not can_complete_setup then
      "<p class='cm-muted-note'>This community is still a setup draft. Its \
       identity and publication are decided together by an authorized \
       publisher.</p>"
    else
      Printf.sprintf
        "<p class='cm-muted-note'>This community is still a private setup \
         draft. Its name, address, description, visibility, and discovery are \
         decided together when you <a href='/c/%s/setup'>complete setup and \
         publish</a>.</p>"
        slug
  in

  let stat label cls value =
    Printf.sprintf
      "<div class='cm-stat'><span class='cm-stat-label'>%s</span><span class='cm-stat-val %s'>%s</span></div>"
      label cls value
  in

  let sort_option value label selected_val =
    let sel = if selected_val = value then " selected" else "" in
    Printf.sprintf "<option value='%s'%s>%s</option>" value sel label
  in

  (* Slice H: per-child indexability state + (TM/A only) toggle. Indexability is sensitive
     (it governs public discovery + chat provenance leakage), so the editing form renders only
     for top_mod/admin — regular mods get the read-only badge. The matching POST handlers re-check
     TM/A regardless. The hidden field carries the explicit next state. In the compact-row layout
     the badge lives in the always-visible summary and the toggle in the expanded ops strip. *)
  let can_edit_idx = is_top_mod || is_admin in
  let idx_badge ~indexable =
    if indexable then "<span class='cm-badge cm-badge--active'>Indexable</span>"
    else "<span class='cm-badge cm-badge--archived'>Not indexable</span>"
  in
  let idx_toggle_form ~action ~indexable =
    if not can_edit_idx then ""
    else
      let (next_val, label) =
        if indexable then ("false", "Make non-indexable") else ("true", "Make indexable")
      in
      Printf.sprintf "<form action='%s' method='POST' class='cm-form-inline'>%s<input type='hidden' name='indexable' value='%s'><button type='submit' class='cm-btn-sm'>%s</button></form>"
        action csrf_token next_val label
  in
  (* Community-level private / non-indexable dominates child flags (Slice G effective_indexable_child):
     while dominated, a child's stored flag has no public effect. We still allow toggling so the flag
     is ready when the community becomes public+indexable; the note below says so plainly. *)
  let community_dominates_children = is_private || not community.indexable in
  let dominated_note =
    if not community_dominates_children then ""
    else if is_private then
      "<p class='cm-muted-note' style='margin:0 0 10px'>This community is private, so everything in it is non-indexable no matter what these flags say. They take effect only once the community is public and indexable.</p>"
    else
      "<p class='cm-muted-note' style='margin:0 0 10px'>This community is public but excluded from discovery (noindex), so everything in it is non-indexable no matter what these flags say. They take effect only once the community is also indexable.</p>"
  in

  (* ---- Panel: Visibility & discovery (default) ---- *)
  (* Replaces the old Overview + Visibility pair. The status strip is read-only and derived
     from data the handler already passes (no fabricated metric). While private, indexability
     is never an actionable control — private communities are never indexed, so the stored
     flag is shown only as inactive secondary copy. Slice E routes/inputs are unchanged. *)
  let visibility_panel =
    let downvotes_row =
      if community.allow_downvotes then stat "downvotes" "cm-stat-val--ok" "enabled"
      else stat "downvotes" "cm-stat-val--off" "disabled"
    in
    let discovery_row =
      if is_private then stat "discovery" "cm-stat-val--off" "noindex &middot; private"
      else if community.indexable then stat "discovery" "cm-stat-val--ok" "indexable"
      else stat "discovery" "cm-stat-val--off" "noindex"
    in
    let status_strip =
      Printf.sprintf "<div class='cm-stats' style='margin-bottom:14px'>%s%s%s%s%s%s</div>"
        (stat "visibility" "" (if is_private then "Private" else "Public"))
        discovery_row
        (stat "live channels" "" (string_of_int active_channel_count))
        (if community.sections_enabled
         then stat "sections" "" (string_of_int (List.length sections))
         else stat "sections" "cm-stat-val--off" "off")
        (stat "moderators" "" (string_of_int (List.length mods)))
        downvotes_row
    in
    let explanation =
      if is_private then
        "<p class='cm-panel-desc'>Private communities are only readable by members, moderators, and admins. \
         <strong>Private communities are never indexed.</strong></p>"
      else if community.indexable then
        "<p class='cm-panel-desc'>This community can appear in the public feed, search, and discovery, and may be indexed by search engines.</p>"
      else
        "<p class='cm-panel-desc'>This community is public by link, but <strong>excluded from the public feed, search, and discovery</strong>, and marked <code>noindex</code>.</p>"
    in
    let visibility_control =
      if is_network_draft then setup_pointer_note
      else if not can_edit_vis then ""
      else
        (* Segmented submit control: each segment is its own one-input POST form to the
           existing route, so the control stays fully server-rendered. Clicking the
           already-active segment re-submits the current value (idempotent). *)
        let seg value label active =
          Printf.sprintf "<form action='/c/%s/settings/visibility' method='POST' class='cm-form-inline'>%s<input type='hidden' name='visibility' value='%s'><button type='submit' class='cm-seg-btn%s'>%s</button></form>"
            slug csrf_token value (if active then " cm-seg-btn--active" else "") label
        in
        Printf.sprintf "
          <div class='cm-field' style='margin-top:2px'>
            <label class='cm-label'>Visibility</label>
            <div class='cm-seg'>%s%s</div>
          </div>"
          (seg "public" "Public" (not is_private))
          (seg "private" "Private" is_private)
    in
    let indexable_control =
      (* Indexing on a network community is not an independent switch: the
         published shapes are "indexable and discoverable" or neither, and
         the legacy route can only move one of the two. It refuses a network
         community for exactly that reason, so no control is offered. *)
      if community.is_network_community then
        if is_network_draft then ""
        else
          "<p class='cm-muted-note' style='margin-top:12px'>Discovery for this \
           community follows the Public or Unlisted choice made when it was \
           published.</p>"
      else if not can_edit_vis then ""
      else if is_private then
        (* No actionable indexability control while private; surface the stored flag as
           inactive copy so it doesn't look clickable. *)
        Printf.sprintf
          "<p class='cm-muted-note' style='margin-top:12px'>Private communities are never indexed. The stored indexability flag is <strong>%s</strong> &mdash; it takes effect only if the community becomes public.</p>"
          (if community.indexable then "indexable" else "non-indexable")
      else
        let seg value label active =
          Printf.sprintf "<form action='/c/%s/settings/indexability' method='POST' class='cm-form-inline'>%s<input type='hidden' name='indexable' value='%s'><button type='submit' class='cm-seg-btn%s'>%s</button></form>"
            slug csrf_token value (if active then " cm-seg-btn--active" else "") label
        in
        Printf.sprintf "
          <div class='cm-field' style='margin-top:14px'>
            <label class='cm-label'>Discovery</label>
            <div class='cm-seg'>%s%s</div>
            <p class='cm-muted-note' style='margin:6px 0 0'>Non-indexable keeps the community readable by link but marked <code>noindex</code> and out of the public feed, search, and discovery.</p>
          </div>"
          (seg "true" "Indexable" community.indexable)
          (seg "false" "Non-indexable" (not community.indexable))
    in
    let editor_note =
      if can_edit_vis then ""
      else "<p class='cm-muted-note'>Only Top Mods and admins can change visibility and discovery settings.</p>"
    in
    Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Visibility &amp; discovery</h2>
        <p class='cm-panel-desc'>Who can read this community, and whether it appears in the public feed, search, and discovery.</p>
        %s
        %s
        %s
        %s
        %s
      </section>"
      status_strip explanation visibility_control indexable_control editor_note
  in

  (* ---- Panel: Profile ---- *)
  (* Native file inputs can't reflect an existing upload ("No file chosen" even when an
     avatar exists), so each upload field gets an explicit current-state row: a preview via
     the shared safe_img_src helpers when an asset exists, or a "none yet" note. Upload
     behavior (multipart /update-community + existing_* fallbacks) is unchanged. *)
  let avatar_status =
    match community.avatar_url with
    | Some u when String.trim u <> "" ->
        Printf.sprintf "<div class='cm-asset'>%s<span class='cm-asset-note'>Current avatar uploaded &mdash; choose a file to replace it.</span></div>"
          (Components.community_avatar ~img_class:"cm-asset-avatar" ~tile_class:"cm-asset-avatar" ~name:community.name (Some u))
    | _ -> "<div class='cm-asset'><span class='cm-asset-note'>No avatar uploaded yet.</span></div>"
  in
  let banner_status =
    match community.banner_url with
    | Some u when String.trim u <> "" ->
        Printf.sprintf "<div class='cm-asset'>%s<span class='cm-asset-note'>Current banner uploaded &mdash; choose a file to replace it.</span></div>"
          (Components.community_banner ~wrap_class:"cm-asset-banner" ~img_class:"cm-asset-banner-img" ~fallback_class:"cm-asset-banner" (Some u))
    | _ -> "<div class='cm-asset'><span class='cm-asset-note'>No banner uploaded yet.</span></div>"
  in
  let profile_panel =
    if is_network_draft then
      (* The legacy multipart route writes the description straight to the
         community row; for a setup draft that is a canonical-identity edit
         outside the publication flow, and the route now refuses it. No form
         is rendered rather than one the server would reject. *)
      Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Profile</h2>
        <p class='cm-panel-desc'>Description, rules, and imagery shown on the public community page.</p>
        %s
      </section>"
        setup_pointer_note
    else
    Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Profile</h2>
        <p class='cm-panel-desc'>Description, rules, and imagery shown on the public community page.</p>
        <form action='/update-community' method='POST' enctype='multipart/form-data' class='cm-form'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='hidden' name='community_slug' value='%s'>
          <div class='cm-field'>
            <label class='cm-label'>Description</label>
            <textarea name='description' rows='3' class='cm-textarea'>%s</textarea>
          </div>
          <div class='cm-field'>
            <label class='cm-label'>Community rules</label>
            <textarea name='rules' rows='5' class='cm-textarea'>%s</textarea>
          </div>
          <div class='cm-assets2'>
            <div class='cm-field'>
              <label class='cm-label'>Avatar image</label>
              %s
              <input type='hidden' name='existing_avatar_url' value='%s'>
              <input type='file' name='avatar_url' accept='image/*' class='cm-file'>
            </div>
            <div class='cm-field'>
              <label class='cm-label'>Banner image</label>
              %s
              <input type='hidden' name='existing_banner_url' value='%s'>
              <input type='file' name='banner_url' accept='image/*' class='cm-file'>
            </div>
          </div>
          <button type='submit' class='cm-btn'>Save changes</button>
        </form>
      </section>"
      csrf_token community.id slug
      (esc (Option.value ~default:"" community.description))
      (esc (Option.value ~default:"" community.rules))
      avatar_status
      (esc (Option.value ~default:"" community.avatar_url))
      banner_status
      (esc (Option.value ~default:"" community.banner_url))
  in

  (* ---- Panel: Channels & sections ---- *)
  (* One compact <details> row per channel/section: current order, name, slug, badges in the
     always-visible summary; the update form and the indexability/archive/delete actions in the
     collapsed body. All routes, methods, and input names are unchanged from the old
     always-expanded cards. idx is the 0-based list index — the position columns exist in the
     schema but have no reorder endpoint, so we only SHOW the current order here; explicit
     reordering is a follow-up. *)
  let render_channel_item idx (c : Db.channel) =
    let status_badge =
      if c.is_archived then "<span class='cm-badge cm-badge--archived'>Archived</span>"
      else "<span class='cm-badge cm-badge--active'>Active</span>"
    in
    (* Active channels link to their live view; archived ones are not navigable. *)
    let name_html =
      if c.is_archived then Printf.sprintf "<span class='cm-section-name'>%s</span>" (esc c.name)
      else Printf.sprintf "<a class='cm-section-name cm-channel-link' href='/c/%s/ch/%s'>%s</a>" slug (esc c.slug) (esc c.name)
    in
    let row_note =
      if c.slug = "general" then "<span class='cm-item-note'>Default channel</span>" else ""
    in
    (* Archive is reversible (never a hard-delete). The default #general channel and the
       last remaining active channel cannot be archived — noted in the ops strip, enforced
       server-side in archive_channel_handler either way. *)
    let archive_control =
      if c.is_archived then
        Printf.sprintf "
          <form action='/c/%s/channels/%d/unarchive' method='POST' class='cm-form-inline'>
            %s
            <button type='submit' class='cm-btn-sm cm-btn-sm--ok'>Unarchive</button>
          </form>"
          slug c.id csrf_token
      else if c.slug = "general" then "<span class='cm-muted-note'>The default #general channel cannot be archived.</span>"
      else if active_channel_count <= 1 then "<span class='cm-muted-note'>The last active channel cannot be archived.</span>"
      else
        Printf.sprintf "
          <form action='/c/%s/channels/%d/archive' method='POST' class='cm-form-inline'
                onsubmit=\"confirmModal(event, 'Archive this channel? Members will no longer see it. You can unarchive it later.')\">
            %s
            <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Archive</button>
          </form>"
          slug c.id csrf_token
    in
    Printf.sprintf "
      <details class='cm-item'>
        <summary>
          <span class='cm-item-pos'>%02d</span>
          <span class='cm-item-name'>%s<span class='cm-section-slug'>/ch/%s</span></span>
          <span class='cm-item-badges'>%s</span>
          <span class='cm-item-meta'>%s<span class='cm-item-hint'>edit</span></span>
        </summary>
        <div class='cm-item-body'>
          <form action='/c/%s/channels/%d/update' method='POST' class='cm-form'>
            %s
            <div class='cm-field'>
              <input type='text' name='name' value='%s' required placeholder='Channel name' class='cm-input'>
            </div>
            <div class='cm-field'>
              <input type='text' name='topic' value='%s' placeholder='Topic (optional)' class='cm-input'>
            </div>
            <button type='submit' class='cm-btn cm-btn--secondary'>Save</button>
          </form>
          <div class='cm-item-ops'>
            %s
          </div>
        </div>
      </details>"
      (idx + 1)
      name_html (esc c.slug)
      status_badge
      row_note
      slug c.id csrf_token
      (esc c.name)
      (esc (Option.value ~default:"" c.topic))
      archive_control
  in
  let channels_block =
    let rows =
      if channels = [] then "<p class='cm-empty'>No channels yet.</p>"
      else Printf.sprintf "<div class='cm-items'>%s</div>"
        (String.concat "\n" (List.mapi render_channel_item channels))
    in
    Printf.sprintf "
      <h3 class='cm-subhead'>Live chat channels</h3>
      <p class='cm-panel-desc'>Live chat channels for real-time discussion in this community. Archive a channel to remove it from the live channel list; you can unarchive it later. The default <code>#general</code> channel and the last active channel cannot be archived.</p>
      %s
      %s
      <details class='cm-add'>
        <summary>+ Add channel</summary>
        <div class='cm-add-body'>
          <form action='/c/%s/channels/add' method='POST' class='cm-form'>
            %s
            <input type='text' name='name' required placeholder='Channel name' class='cm-input'>
            <input type='text' name='topic' placeholder='Topic (optional)' class='cm-input'>
            <button type='submit' class='cm-btn'>Add channel</button>
          </form>
        </div>
      </details>"
      dominated_note rows slug csrf_token
  in
  let sections_inner =
    if not community.sections_enabled then ""
    else begin
      let render_section_item idx (s : community_section) =
        Printf.sprintf "
          <details class='cm-item'>
            <summary>
              <span class='cm-item-pos'>%02d</span>
              <span class='cm-item-name'><span class='cm-section-name'>%s</span><span class='cm-section-slug'>/s/%s</span></span>
              <span class='cm-item-badges'>%s</span>
              <span class='cm-item-meta'><span class='cm-item-note'>sort: %s</span><span class='cm-item-hint'>edit</span></span>
            </summary>
            <div class='cm-item-body'>
              <form action='/c/%s/sections/%d/update' method='POST' class='cm-form'>
                %s
                <div class='cm-row2'>
                  <input type='text' name='name' value='%s' required class='cm-input'>
                  <select name='default_sort' class='cm-select'>
                    %s%s%s%s
                  </select>
                </div>
                <textarea name='description' rows='2' placeholder='Description (optional)' class='cm-textarea'>%s</textarea>
                <button type='submit' class='cm-btn cm-btn--secondary'>Save</button>
              </form>
              <div class='cm-item-ops'>
                %s
                <form action='/c/%s/sections/%d/delete' method='POST' class='cm-form-inline'
                      onsubmit=\"return confirm('Delete this section? Posts will not be deleted. They will be moved to Uncategorized.')\">
                  %s
                  <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Delete section</button>
                </form>
                <span class='cm-muted-note'>Posts move to Uncategorized on delete.</span>
              </div>
            </div>
          </details>"
          (idx + 1)
          (esc s.name) (esc s.slug)
          (idx_badge ~indexable:s.indexable)
          (esc s.default_sort)
          slug s.section_id csrf_token
          (esc s.name)
          (sort_option "hot" "Hot" s.default_sort)
          (sort_option "new" "New" s.default_sort)
          (sort_option "top" "Top" s.default_sort)
          (sort_option "active" "Active" s.default_sort)
          (esc (Option.value ~default:"" s.description))
          (idx_toggle_form ~action:(Printf.sprintf "/c/%s/sections/%d/indexability" slug s.section_id) ~indexable:s.indexable)
          slug s.section_id csrf_token
      in
      let next_position = List.length sections + 1 in
      let rows =
        if sections = [] then "<p class='cm-empty'>No sections yet.</p>"
        else Printf.sprintf "<div class='cm-items'>%s</div>"
          (String.concat "\n" (List.mapi render_section_item sections))
      in
      Printf.sprintf "
        <h3 class='cm-subhead' style='margin-top:24px'>Forum sections</h3>
        <p class='cm-panel-desc'>Organize posts into sections. Deleting a section moves its posts to Uncategorized. Indexable sections and their threads can appear in the public feed, search, and discovery; non-indexable ones are marked <code>noindex</code> and excluded from public discovery.</p>
        %s
        %s
        <details class='cm-add'>
          <summary>+ Add section</summary>
          <div class='cm-add-body'>
            <form action='/c/%s/sections/add' method='POST' class='cm-form'>
              %s
              <div class='cm-row2'>
                <input type='text' name='name' required placeholder='Section name' class='cm-input'>
                <select name='default_sort' class='cm-select'>
                  <option value='hot'>Hot</option>
                  <option value='new'>New</option>
                  <option value='top'>Top</option>
                  <option value='active'>Active</option>
                </select>
              </div>
              <textarea name='description' rows='2' placeholder='Description (optional)' class='cm-textarea'></textarea>
              <input type='hidden' name='position' value='%d'>
              <button type='submit' class='cm-btn'>Add section</button>
            </form>
          </div>
        </details>"
      dominated_note
      rows
      slug csrf_token next_position
    end
  in
  let channels_panel =
    Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Channels &amp; sections</h2>
        <p class='cm-panel-desc'>Live chat channels and forum sections for this community. Rows are shown in their current order.</p>
        %s
        %s
      </section>"
      channels_block sections_inner
  in

  (* ---- Panel: Members ---- *)
  (* Slice F: member allow-list management. Adding/removing members is TM/A-only (same gate as
     visibility/indexability), so the controls render only for top_mod/admin; the matching POST
     handlers re-check TM/A. Regular mods see a read-only list + a note. This manages ONLY
     community_members — moderators/admins keep read access through their role, so they may not
     appear here. The add form renders only while the community is private: for a public
     community the allow-list has no access effect, so offering an active add control would be
     misleading. Existing rows keep their remove action either way (same route/inputs). *)
  let members_panel =
    let can_edit = is_top_mod || is_admin in
    let desc =
      if is_private then
        "<p class='cm-panel-desc'>Members can read this private community. Top Mods and admins can add or remove members. \
          Removing a member is not a ban &mdash; it only removes them from the member list. \
          Moderators and admins may still have access through their role, so they may not appear in this list.</p>"
      else
        "<p class='cm-panel-desc'>This community is public. Members listed here are saved community members, but access is not restricted while the community is public. \
          Removing a member only deletes the saved member entry &mdash; it does not ban them. \
          The member list takes effect again if the community becomes private.</p>"
    in
    let member_rows =
      if members = [] then
        "<p class='cm-empty'>No members in the allow-list yet.</p>"
      else
        let rows = String.concat "\n" (List.map (fun (m : user) ->
          let remove_btn =
            if not can_edit then ""
            else
              Printf.sprintf "
                <form action='/c/%s/settings/members/remove' method='POST' class='cm-form-inline'>
                  %s
                  <input type='hidden' name='target_user_id' value='%d'>
                  <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Remove member</button>
                </form>"
                slug csrf_token m.id
          in
          Printf.sprintf "
            <div class='cm-list-row'>
              <a href='/u/%s' class='cm-user-link'>u/%s</a>
              %s
            </div>"
            (esc m.username) (esc m.username) remove_btn
        ) members) in
        Printf.sprintf "<div class='cm-list'>%s</div>" rows
    in
    let add_form =
      if not is_private then ""
      else if not can_edit then
        "<p class='cm-muted-note'>Only Top Mods and admins can manage members.</p>"
      else
        Printf.sprintf "
          <form action='/c/%s/settings/members/add' method='POST' class='cm-inline-form' style='margin-top:16px'>
            %s
            <input type='text' name='username' required placeholder='Username to add' class='cm-input'>
            <button type='submit' class='cm-btn'>Add member</button>
          </form>"
          slug csrf_token
    in
    Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Members</h2>
        %s
        %s
        %s
      </section>"
      desc member_rows add_form
  in

  (* ---- Panel: Moderation ---- *)
  (* The cross-surface cardlinks (manage-mods, reports, modlog) plus the downvote
     enable/disable, grouped as one moderation area. The toggle is display-gated to
     top_mod/admin as before; toggle_downvotes_handler remains the authority and re-checks
     top_mod/admin. Route, POST method, CSRF, and the allow_downvotes input name/values are
     preserved exactly. *)
  let moderation_panel =
    let modtools_block =
      if not (is_top_mod || is_admin) then ""
      else
        (* Same segmented idiom as visibility/discovery; each segment POSTs the explicit
           allow_downvotes value to the existing route. *)
        let seg value label active =
          Printf.sprintf "<form action='/c/%s/toggle_downvotes' method='POST' class='cm-form-inline'>%s<input type='hidden' name='allow_downvotes' value='%s'><button type='submit' class='cm-seg-btn%s'>%s</button></form>"
            slug csrf_token value (if active then " cm-seg-btn--active" else "") label
        in
        Printf.sprintf "
          <h3 class='cm-subhead' style='margin-top:0'>Moderation tools</h3>
          <div class='cm-field'>
            <label class='cm-label'>Downvotes</label>
            <div class='cm-seg'>%s%s</div>
            <p class='cm-muted-note' style='margin:6px 0 0'>Only Top Mods and admins can change this.</p>
          </div>"
          (seg "true" "Enabled" community.allow_downvotes)
          (seg "false" "Disabled" (not community.allow_downvotes))
    in
    Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Moderation</h2>
        <p class='cm-panel-desc'>Governance, flagged content, and the public audit trail for this community.</p>
        <div class='cm-modlinks'>
          <a href='/c/%s/manage-mods' class='cm-cardlink cm-cardlink--sm'>
            <div>
              <p class='cm-cardlink-title'>Manage moderators</p>
              <p class='cm-cardlink-desc'>Add, promote, and remove moderators &mdash; Council of Equals governance.</p>
            </div>
            <span class='cm-cardlink-go'>&rarr;</span>
          </a>
          <a href='/c/%s/reports' class='cm-cardlink cm-cardlink--sm'>
            <div>
              <p class='cm-cardlink-title'>Reports%s</p>
              <p class='cm-cardlink-desc'>Review posts and comments flagged by members &mdash; spam, abuse, and rule-breaking content.</p>
            </div>
            <span class='cm-cardlink-go'>&rarr;</span>
          </a>
          <a href='/c/%s/modlog' class='cm-cardlink cm-cardlink--sm'>
            <div>
              <p class='cm-cardlink-title'>Mod log</p>
              <p class='cm-cardlink-desc'>Public audit trail of moderation actions in this community.</p>
            </div>
            <span class='cm-cardlink-go'>&rarr;</span>
          </a>
        </div>
        %s
      </section>"
      slug
      slug
      (if open_reports_count > 0
       then Printf.sprintf " <span class='cm-badge cm-badge--active'>%d open</span>" open_reports_count
       else "")
      slug
      modtools_block
  in

  (* ---- Panel: Bans ---- *)
  let bans_panel =
    let banned_section =
      if banned_users = [] then
        "<p class='cm-empty'>No users are currently banned from this community.</p>"
      else
        let rows = String.concat "\n" (List.map (fun (b : user) ->
          Printf.sprintf "
            <div class='cm-list-row'>
              <a href='/u/%s' class='cm-user-link'>u/%s</a>
              <form action='/unban-community-user' method='POST' class='cm-form-inline'>
                %s
                <input type='hidden' name='target_user_id' value='%d'>
                <input type='hidden' name='community_id' value='%d'>
                <input type='hidden' name='community_slug' value='%s'>
                <button type='submit' class='cm-btn-sm cm-btn-sm--ok'>Unban</button>
              </form>
            </div>"
            (esc b.username) (esc b.username) csrf_token b.id community.id slug
        ) banned_users) in
        Printf.sprintf "<div class='cm-list'>%s</div>" rows
    in
    Printf.sprintf "
      <section class='cm-panel cm-danger'>
        <h2 class='cm-panel-title'>Bans</h2>
        <p class='cm-panel-desc'>Banned users cannot post or comment in this community. Bans are logged to the mod log.</p>
        <form action='/ban-community-user' method='POST' class='cm-inline-form'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='text' name='target_username' required placeholder='Username to ban' class='cm-input'>
          <button type='submit' class='cm-btn'>Ban user</button>
        </form>
        <h3 class='cm-subhead'>Banned users</h3>
        %s
      </section>"
      csrf_token community.id banned_section
  in

  (* Already-escaped, form-bearing HTML from the pure removal-pages module; this page adds
     only the panel chrome and never inspects, rewrites, or re-escapes it. *)
  let projects_panel =
    Printf.sprintf
      "<section class='cm-panel'>
        <h2 class='cm-panel-title'>Connected projects</h2>
        <p class='cm-panel-desc'>Open-source projects that use this community as their Earde home.</p>
        %s
      </section>"
      connected_projects
  in

  let main_panel =
    match panel with
    | "profile" -> profile_panel
    | "channels" -> channels_panel
    | "members" -> members_panel
    | "moderation" -> moderation_panel
    | "projects" -> projects_panel
    | "bans" -> bans_panel
    | _ -> visibility_panel
  in

  (* Left nav: real panel navigation (GET links back to this route), not on-page anchors. *)
  let nav_item ?(danger=false) key label =
    let active_cls = if panel = key then " cm-index-link--active" else "" in
    let danger_cls = if danger then " cm-index-link--danger" else "" in
    Printf.sprintf "<a class='cm-index-link%s%s' href='/c/%s/settings?panel=%s'>%s</a>"
      danger_cls active_cls slug key label
  in
  (* Project home requests queue: a normal GET link to the dedicated
     moderator route (its own handler + read model still authorize). Shown
     only on the top-mod/admin surface — regular mods, whom this page
     already knows are unauthorized for the queue, never see it. No badge or
     pending count, no form, no community id. The URL is built structurally
     from the canonical (escaped) slug, matching the other nav links. *)
  let project_home_requests_link =
    if is_top_mod || is_admin then
      Printf.sprintf
        "<a class='cm-index-link' href='/c/%s/project-home-requests'>Project home requests</a>"
        slug
    else ""
  in
  (* Connected projects: an ordinary panel nav entry, present only when the route supplied
     the management fragment (top-mod/admin surface). Regular mods, whom this page already
     knows are unauthorized for project-home moderation, never see it. *)
  let connected_projects_nav =
    if has_connected_projects then nav_item "projects" "Connected projects" else ""
  in
  (* Complete setup and publish: a normal GET link to the dedicated setup
     route, which independently reauthorizes (current top_mod of this
     community, or a durable users.is_admin holder) and independently
     re-checks the draft lifecycle. Shown only on a network setup draft's
     top-mod/admin surface — legacy communities, already-published network
     communities, ordinary members, mods, and legacy_mods never see it. No
     form, no community id, no lifecycle detail; the URL is built
     structurally from the canonical (escaped) slug, matching the other nav
     links. *)
  let setup_publish_link =
    if can_complete_setup then
      Printf.sprintf
        "<a class='cm-index-link' href='/c/%s/setup'>Complete setup and publish</a>"
        slug
    else ""
  in
  let content = Printf.sprintf "
    <div class='cm-wrap cm-wrap--settings'>
      <div class='cm-head'>
        <h1 class='cm-h1'>&#x2699;&#xFE0F; /c/%s <span class='accent'>settings</span></h1>
        <a href='/c/%s' class='cm-back'>&larr; Back to community</a>
      </div>

      <div class='cm-cols'>
        <nav class='cm-index'>
          <div class='cm-index-title'>Settings</div>
          %s%s%s%s%s%s%s%s%s
        </nav>
        <div class='cm-main'>
          %s
        </div>
      </div>
    </div>"
    slug slug
    setup_publish_link
    (nav_item "visibility" "Visibility &amp; discovery")
    (nav_item "profile" "Profile")
    (nav_item "channels" "Channels &amp; sections")
    (nav_item "members" "Members")
    (nav_item "moderation" "Moderation")
    connected_projects_nav
    project_home_requests_link
    (nav_item ~danger:true "bans" "Bans")
    main_panel
  in
  (* Cartographic launch shell (pass 11A): the same four-pane chrome as the
     overview/channel/section/thread routes, wrapping the settings content
     verbatim — every cm-* fragment above is test-pinned or treated as such,
     so only this outer document changed. The sidebar reuses the shared
     knowledge grammar with Settings active; can_manage is true by
     construction (the handler already 403'd everyone below moderator). The
     Home requests queue entry stays exclusively in the settings index above:
     its canonical link is pinned to exactly one occurrence per document, so
     the sidebar must not repeat it. [rail_communities] carries the viewer's
     joined communities exactly as the handler loaded them (post-authorization
     only); ordering, dedup by slug, and the single active marker stay owned
     by the shared launch doc builder, so the settings rail can never diverge
     from the overview/channel/section/thread rails. Replay privacy keeps the
     existing inner ph-no-capture guard for private communities (the launch
     shell additionally marks .shell, exactly like the sibling routes). *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ~settings_active:true ~can_manage:true ()
  in
  Components.launch_community_page ?user ~request ~rail_communities ~community ~sidebar
    ~page_class:"launch-community-settings"
    ~title:(Printf.sprintf "Settings — /c/%s" community.slug)
    ~content:(Components.private_replay_guard ~community content) ()

let manage_mods_page ?user ?(rail_communities = []) ~is_admin ~current_user_role
    ~(channels : channel list) ~(sections : community_section list)
    ~(community : community) ~(mods : moderator_entry list) request =
  let csrf_token = Dream.csrf_tag request in
  let esc = Components.html_escape in
  let slug = esc community.slug in

  (* Group mods by role for visual separation. *)
  let top_mods   = List.filter (fun m -> m.role = "top_mod")   mods in
  let regular_mods = List.filter (fun m -> m.role = "mod")     mods in
  let legacy_mods  = List.filter (fun m -> m.role = "legacy_mod") mods in

  let can_manage = is_admin || current_user_role = Some "top_mod" in

  let render_top_mod_row (m : moderator_entry) =
    (* A top_mod cannot demote another top_mod; only admins have that power.
       Prevents power consolidation by a single top_mod ousting peers. *)
    let action_btn =
      if is_admin then
        Printf.sprintf "
          <form action='/c/%s/manage-mods/remove' method='POST' class='cm-form-inline' onsubmit=\"confirmModal(event, 'Demote this Top Mod? They will be removed from the council.')\">
            %s
            <input type='hidden' name='target_user_id' value='%d'>
            <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Remove</button>
          </form>"
          slug csrf_token m.user_id
      else "<span class='cm-muted-note'>Top Mod</span>"
    in
    Printf.sprintf "
      <div class='cm-list-row'>
        <div><a href='/u/%s' class='cm-user-link'>u/%s</a><span class='cm-badge cm-badge--top'>Top Mod</span></div>
        %s
      </div>"
      (esc m.username) (esc m.username) action_btn
  in

  let render_mod_row (m : moderator_entry) =
    let action_btns =
      if can_manage then
        Printf.sprintf "
          <div class='cm-row-actions'>
            <form action='/c/%s/manage-mods/promote' method='POST' class='cm-form-inline'>
              %s
              <input type='hidden' name='target_user_id' value='%d'>
              <button type='submit' class='cm-btn-sm cm-btn-sm--promote'>Promote to Top Mod</button>
            </form>
            <form action='/c/%s/manage-mods/remove' method='POST' class='cm-form-inline' onsubmit=\"confirmModal(event, 'Remove this moderator?')\">
              %s
              <input type='hidden' name='target_user_id' value='%d'>
              <button type='submit' class='cm-btn-sm cm-btn-sm--danger'>Remove</button>
            </form>
          </div>"
          slug csrf_token m.user_id
          slug csrf_token m.user_id
      else ""
    in
    Printf.sprintf "
      <div class='cm-list-row'>
        <a href='/u/%s' class='cm-user-link'>u/%s</a>
        %s
      </div>"
      (esc m.username) (esc m.username) action_btns
  in

  let render_legacy_row (m : moderator_entry) =
    Printf.sprintf "
      <div class='cm-list-row'>
        <div><a href='/u/%s' class='cm-user-link cm-user-link--muted'>u/%s</a><span class='cm-badge cm-badge--legacy'>Legacy</span></div>
        <span class='cm-muted-note'>No active powers</span>
      </div>"
      (esc m.username) (esc m.username)
  in

  let top_mod_section =
    if top_mods = [] then "<p class='cm-empty'>No Top Mods yet.</p>"
    else Printf.sprintf "<div class='cm-list'>%s</div>" (String.concat "\n" (List.map render_top_mod_row top_mods))
  in
  let mod_section =
    if regular_mods = [] then "<p class='cm-empty'>No standard moderators.</p>"
    else Printf.sprintf "<div class='cm-list'>%s</div>" (String.concat "\n" (List.map render_mod_row regular_mods))
  in
  let legacy_section =
    if legacy_mods = [] then ""
    else Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Legacy moderators</h2>
        <p class='cm-panel-desc'>Demoted due to inactivity. No permissions granted.</p>
        <div class='cm-list'>%s</div>
      </section>" (String.concat "\n" (List.map render_legacy_row legacy_mods))
  in

  let add_mod_form =
    if can_manage then Printf.sprintf "
      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Add new moderator</h2>
        <form action='/c/%s/manage-mods/add' method='POST' class='cm-inline-form'>
          %s
          <input type='text' name='username' required placeholder='Username' class='cm-input'>
          <button type='submit' class='cm-btn'>Add mod</button>
        </form>
      </section>" slug csrf_token
    else ""
  in

  let content = Printf.sprintf "
    <div class='cm-wrap'>
      <div class='cm-head'>
        <h1 class='cm-h1'>Council of Mods <span class='accent'>&mdash; /c/%s</span></h1>
        <a href='/c/%s/settings?panel=moderation' class='cm-back'>&larr; Back to settings</a>
      </div>

      %s

      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Top Mods</h2>
        <p class='cm-panel-desc'>Council seats (max 3). Only admins can remove Top Mods.</p>
        %s
      </section>

      <section class='cm-panel'>
        <h2 class='cm-panel-title'>Moderators</h2>
        %s
      </section>

      %s
    </div>"
    slug slug
    add_mod_form
    top_mod_section
    mod_section
    legacy_section
  in
  (* Cartographic launch shell (pass 14C): the same four-pane chrome as the
     sibling community routes, wrapping the moderator roster verbatim — the
     cm-* role sections, the add/promote/remove forms with their hidden
     target_user_id fields, the confirmModal onsubmit hooks (the shared
     launch behavior script carries confirmModal and the #modal-confirm-msg /
     #cancel-btn / #confirm-btn contract) and the role badges are treated as
     pinned, so only this outer document changed. The sidebar reuses the
     shared knowledge grammar with Manage moderators active; can_manage is
     true by construction here — the handler only renders this page for TM/A
     viewers, exactly the settings gate. [rail_communities] carries the
     viewer's joined communities as the handler loaded them
     (post-authorization only); ordering, dedup by slug and the single active
     marker stay owned by the shared launch doc builder. Replay privacy keeps
     the existing inner ph-no-capture guard for private communities (the
     launch shell additionally marks .shell, like the sibling routes; the
     launch doc also owns the analytics assets with the same
     community-id/visibility pair the legacy wrapper received). *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ~manage_moderators_active:true ~can_manage:true ()
  in
  Components.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-moderators"
    ~title:(Printf.sprintf "Manage Mods — /c/%s" community.slug)
    ~content:(Components.private_replay_guard ~community content) ()

(* === POST === *)

(* The three /new-post states below (community chooser, join gate, creation
   form) moved onto the Cartographic launch shell (pass 15B). The create-*
   fragments are kept verbatim — every field name/id/value, the hidden
   community_id, the section <select>, the Dream CSRF tag and the legacy
   .create-shell marker are unchanged — and only the outer document swapped
   Components.create_page → Components.launch_app_page (page_class
   launch-post-creation), which owns the topbar, dark rail, analytics assets
   and the member behavior script. [rail_communities] carries the viewer's
   joined communities as the handler loaded them (post-authorization only);
   ordering and the rail tiles stay owned by the shared launch doc. The
   community-bound states keep their (id, visibility) analytics pair through
   the wrapper's [analytics_community], exactly what create_page received. *)
let choose_community_page ?user ?request ?(rail_communities = []) (communities : community list) =
  let render_option (community : community) =
    Printf.sprintf "
    <a href='/new-post?community=%s' class='create-comm'>
        <span>
            <span class='create-comm-name'>%s</span>
            <span class='create-comm-slug'>/c/%s</span>
        </span>
        <span class='create-comm-go'>Post here &rarr;</span>
    </a>"
    community.slug (Components.html_escape community.name) community.slug
  in

  let list_html = String.concat "\n" (List.map render_option communities) in

  let content = Printf.sprintf "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Choose a community</h1>
            <p class='create-sub'>Select where you want to publish your post.</p>
        </div>

        <div class='create-list'>
            %s
        </div>

        <div class='create-foot'>
            Can't find the right place? <a href='/bring' class='create-link'>Connect a project</a>
        </div>
      </div>
    </div>"
    list_html
  in
  Components.launch_app_page ?user ?request ~rail_communities
    ~page_class:"launch-post-creation" ~title:"Choose Community"
    ~content:(Printf.sprintf "<div class='create-shell'>%s</div>" content) ()

let join_to_post_page ?user ?(rail_communities = []) (community : community) request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='create-wrap create-wrap--narrow'>
      <div class='create-panel'>
        <div class='create-gate'>
            <div class='create-gate-icon'>
                <svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><rect x='3' y='11' width='18' height='11' rx='2'/><path d='M7 11V7a5 5 0 0110 0v4'/></svg>
            </div>
            <h1 class='create-title'>Members only</h1>
            <p class='create-gate-text'>
                You must be a member of <span class='create-gate-name'>/c/%s</span> to create a post here.
            </p>

            <form action='/join' method='POST' class='create-form'>
                %s
                <input type='hidden' name='community_id' value='%d'>
                <input type='hidden' name='redirect_to' value='/new-post?community=%s'>

                <button type='submit' class='create-btn create-btn--block'>Join community &amp; post</button>
            </form>

            <div class='create-foot'>
                <a href='/' class='create-link'>Cancel and return home</a>
            </div>
        </div>
      </div>
    </div>"
    (Components.html_escape community.slug) csrf_token community.id community.slug
  in
  Components.launch_app_page ?user ~request ~rail_communities
    ~analytics_community:(community.id, community.visibility)
    ~page_class:"launch-post-creation" ~title:("Join " ^ community.name)
    ~content:(Printf.sprintf "<div class='create-shell'>%s</div>" content) ()

(* GET form for "Start thread from chat". Mirrors new_post_form's create-* shell so it
   inherits the community shell language (no legacy Site chrome). The seed message is
   shown selected + locked; nearby messages are checkboxes; the thread body is editable
   notes. SSR, normal POST, CSRF. No JS. *)
let start_thread_form ?user ?error ~(community : community) ~(channel : channel)
    ~(seed_id : int64) ~(candidates : (chat_message * string option) list)
    ~(sections : community_section list)
    ~(default_section_id : int) ~default_title ~default_body request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let action = Printf.sprintf "/c/%s/ch/%s/messages/%Ld/start-thread"
    (esc community.slug) (esc channel.slug) seed_id in
  let section_dropdown =
    if sections = [] then ""
    else begin
      let options = String.concat "\n" (List.map (fun (s : community_section) ->
        let selected = if s.section_id = default_section_id then " selected" else "" in
        Printf.sprintf "<option value='%d'%s>%s</option>" s.section_id selected (esc s.name)
      ) sections) in
      Printf.sprintf "
            <div class='create-field'>
                <label class='create-label'>Section <span class='req'>*</span></label>
                <select name='section_id' required class='create-select'>%s</select>
            </div>" options
    end
  in
  let render_candidate ((m : chat_message), (author : string option)) =
    let name = match author with Some u -> u | None -> "[deleted]" in
    let is_seed = m.id = seed_id in
    let checkbox =
      if is_seed then
        Printf.sprintf "<input type='checkbox' checked disabled class='st-check'><input type='hidden' name='seed' value='%Ld'>" m.id
      else
        Printf.sprintf "<input type='checkbox' name='msg_%Ld' value='on' class='st-check'>" m.id
    in
    let seed_tag = if is_seed then "<span class='st-seed-tag'>seed</span>" else "" in
    Printf.sprintf
      "<label class='st-msg%s'>%s<span class='st-msg-body'><span class='st-msg-meta'><span class='st-msg-author'>%s</span><span class='st-msg-time'>%s</span>%s</span><span class='st-msg-text'>%s</span></span></label>"
      (if is_seed then " st-msg--seed" else "")
      checkbox (esc name) (esc m.created_at) seed_tag (esc m.content)
  in
  let candidates_html =
    if candidates = [] then "<p class='create-hint'>No nearby messages.</p>"
    else String.concat "\n" (List.map render_candidate candidates)
  in
  let error_html = match error with
    | Some e when e <> "" -> Printf.sprintf "<div class='st-error'>%s</div>" (esc e)
    | _ -> "" in
  let content = Printf.sprintf "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Start a thread from <span class='accent'>#%s</span></h1>
            <p class='create-sub'>Crystallize this chat conversation into a durable forum thread. The messages you select are attached as source context on the thread.</p>
        </div>
        %s
        <form action='%s' method='POST' class='create-form'>
            %s
            <div class='create-field'>
                <label class='create-label'>Source messages</label>
                <p class='create-hint'>The seed message is always included. Tick nearby messages to attach (up to 10 total). <span id='st-selection-count'></span></p>
                <div class='st-msglist'>%s</div>
            </div>
            %s
            <div class='create-field'>
                <label class='create-label'>Title <span class='req'>*</span></label>
                <input type='text' name='title' required maxlength='300' class='create-input' value='%s' placeholder='What is this thread about?'>
            </div>
            <div class='create-field'>
                <label class='create-label'>Introduction / context <span class='create-label-opt'>(optional)</span></label>
                <textarea name='content' class='create-textarea' style='min-height:160px;' placeholder='Why is this conversation worth keeping? Add any context for the forum.'>%s</textarea>
                <p class='create-hint'>The selected messages are shown on the thread automatically &mdash; this field is only for your own introduction.</p>
            </div>
            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Start thread</button>
                <a href='/c/%s/ch/%s' class='st-cancel'>Cancel</a>
            </div>
        </form>
      </div>
    </div>
    <script>
    (function () {
      var out = document.getElementById('st-selection-count');
      var list = document.querySelector('.st-msglist');
      if (!out || !list) return;
      function update() {
        var rows = list.querySelectorAll('.st-msg');
        var count = 0;
        var authors = {};
        rows.forEach(function (row) {
          var box = row.querySelector('.st-check');
          if (!box || !box.checked) return;
          count += 1;
          var a = row.querySelector('.st-msg-author');
          if (a) authors[a.textContent] = true;
        });
        var participants = Object.keys(authors).length;
        out.textContent = count + ' of 10 selected · ' +
          participants + (participants === 1 ? ' participant' : ' participants');
      }
      list.addEventListener('change', update);
      update();
    })();
    </script>"
    (esc channel.name)
    error_html
    action
    csrf_token
    candidates_html
    section_dropdown
    (esc default_title)
    (esc default_body)
    (esc community.slug) (esc channel.slug)
  in
  Components.create_page ?user ~request ~analytics_community:(community.id, community.visibility)
    ~title:("Start thread — #" ^ channel.name) ~body:content ()

let new_post_form ?user ?preselected_section_id ?(rail_communities = []) (sections : community_section list) (community : community) request =
  let csrf_token = Dream.csrf_tag request in
  let section_dropdown =
    if sections = [] then ""
    else begin
      let options = String.concat "\n" (List.map (fun (s : community_section) ->
        let selected = match preselected_section_id with
          | Some id when id = s.section_id -> " selected"
          | _ -> ""
        in
        Printf.sprintf "<option value='%d'%s>%s</option>"
          s.section_id selected (Components.html_escape s.name)
      ) sections) in
      Printf.sprintf "
            <div class='create-field'>
                <label class='create-label'>Section <span class='req'>*</span></label>
                <select name='section_id' required class='create-select'>
                    <option value=''>-- Select a section --</option>
                    %s
                </select>
            </div>" options
    end
  in
  let content = Printf.sprintf "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Post to <span class='accent'>/c/%s</span></h1>
            <p class='create-sub'>Share a link, an image, or a text discussion with the community.</p>
        </div>

        <form action='/posts' method='POST' enctype='multipart/form-data' class='create-form'>
            %s
            <input type='hidden' name='community_id' value='%d'>

            %s

            <div class='create-field'>
                <label class='create-label'>Title <span class='req'>*</span></label>
                <input type='text' name='title' required class='create-input' placeholder='An interesting title'>
            </div>

            <div class='create-field'>
                <label class='create-label'>URL <span class='create-label-opt'>(optional)</span></label>
                <input type='url' name='url' class='create-input' placeholder='https://example.com'>
            </div>

            <div class='create-field'>
                <label class='create-label'>Image <span class='create-label-opt'>(optional, max 5 MB)</span></label>
                <input type='file' name='image' accept='image/*' class='create-file'>
                <p class='create-hint'>Converted to WebP automatically. Leave blank for a text or link post.</p>
            </div>

            <div class='create-field'>
                <label class='create-label'>Text <span class='create-label-opt'>(optional)</span></label>
                <textarea name='content' class='create-textarea' style='min-height:140px;' placeholder='Share your thoughts...'></textarea>
            </div>

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Submit post</button>
            </div>
        </form>
      </div>
    </div>"
    (Components.html_escape community.slug)
    csrf_token
    community.id
    section_dropdown
  in
  Components.launch_app_page ?user ~request ~rail_communities
    ~analytics_community:(community.id, community.visibility)
    ~page_class:"launch-post-creation" ~title:("Post to " ^ community.name)
    ~content:(Printf.sprintf "<div class='create-shell'>%s</div>" content) ()

(* SSR report form (no JS). The handler is the security boundary: it re-resolves the
   target from the trusted slug + hidden type/id and re-runs the ban/self-report gates,
   so this page only sets up the inputs. target_type is emitted as the closed-variant
   string; the reason <select> values mirror report_reason_to_string. *)
let report_form_page ?user ?(rail_communities = []) ~(channels : channel list)
    ~(sections : community_section list) ~can_manage ~(community : community)
    ~(target_type : Db.report_target) ~target_id ~target_title ~return_url request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let kind_label = match target_type with
    | Db.Report_post -> "post"
    | Db.Report_comment -> "comment"
    | Db.Report_chat_message -> "message" in
  let target_type_s = Db.report_target_to_string target_type in
  (* Trim an over-long excerpt for the context line; full content stays untouched in the DB. *)
  let excerpt =
    let t = String.trim target_title in
    if String.length t <= 160 then t else String.sub t 0 157 ^ "\xe2\x80\xa6" in
  let excerpt_html =
    if excerpt = "" then ""
    else Printf.sprintf "<div class='create-field'><label class='create-label'>Reported %s</label><p class='create-hint ph-mask'>%s</p></div>"
      kind_label (esc excerpt) in
  let reason_option value label = Printf.sprintf "<option value='%s'>%s</option>" value label in
  let reasons = String.concat "\n" [
    reason_option "spam" "Spam";
    reason_option "abuse" "Abuse / harassment";
    reason_option "off_topic" "Off-topic";
    reason_option "illegal" "Illegal / dangerous";
    reason_option "other" "Other";
  ] in
  let content = Printf.sprintf "
    <div class='create-wrap'>
      <div class='create-panel'>
        <div class='create-head'>
            <h1 class='create-title'>Report this %s</h1>
            <p class='create-sub'>Flag content in <span class='accent'>/c/%s</span> for the moderators. Reports are private; a moderator will review it.</p>
        </div>

        <form action='/c/%s/reports' method='POST' class='create-form'>
            %s
            <input type='hidden' name='target_type' value='%s'>
            <input type='hidden' name='target_id' value='%d'>

            %s

            <div class='create-field'>
                <label class='create-label'>Reason <span class='req'>*</span></label>
                <select name='reason' required class='create-select'>
                    <option value=''>-- Select a reason --</option>
                    %s
                </select>
            </div>

            <div class='create-field'>
                <label class='create-label'>Details <span class='create-label-opt'>(optional)</span></label>
                <textarea name='details' maxlength='1000' class='create-textarea' style='min-height:120px;' placeholder='Add any context that will help a moderator (optional).'></textarea>
                <p class='create-hint'>Up to 1000 characters.</p>
            </div>

            <div class='create-actions'>
                <button type='submit' class='create-btn create-btn--block'>Submit report</button>
                <a href='%s' class='create-btn create-btn--secondary create-btn--block' style='text-align:center;'>Cancel</a>
            </div>
        </form>
      </div>
    </div>"
    kind_label
    (esc community.slug)
    (esc community.slug)
    csrf_token
    target_type_s
    target_id
    excerpt_html
    reasons
    (Components.safe_internal_path return_url)
  in
  (* Cartographic launch shell (pass 14D): the same four-pane chrome as the
     sibling community routes, wrapping the report form verbatim — the create-*
     fields, the hidden target_type/target_id inputs, the reason <select>
     values, the details maxlength and the ph-mask excerpt class are treated
     as pinned, so only this outer document changed (create.css idiom → the
     route-scoped earde.css section). The sidebar reuses the shared knowledge
     grammar with NO active entry: reporting is a contextual action, not a
     permanent sidebar destination, and the moderator-only entries (Reports,
     Manage moderators, Home requests) stay hidden because their active flags
     default to false. can_manage is the handler's real admin-or-moderator
     check, so Settings shows only to authorized viewers — exactly the
     overview's render-time visibility rule; the settings handler re-checks
     authority. [rail_communities] carries the viewer's joined communities as
     the handler loaded them (post-authorization only); ordering, dedup by
     slug and the single active marker stay owned by the shared launch doc
     builder. Replay privacy keeps the inner ph-no-capture guard for private
     communities (the launch shell additionally marks .shell, like the
     sibling routes; the launch doc also owns the analytics assets with the
     same community-id/visibility pair the legacy wrapper received), and the
     legacy wrapper's unconditional noindex is preserved. *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections ~can_manage ()
  in
  Components.launch_community_page ?user ~request ~noindex:true ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-report-form"
    ~title:("Report " ^ kind_label)
    ~content:(Components.private_replay_guard ~community content) ()

(* Read-only community mod queue. [previews] is a (report_id -> (context_url, excerpt))
   assoc the handler built with a bounded per-row lookup; rows missing from it (chat,
   deleted, or beyond the preview cap) degrade to "Target unavailable or deleted". This
   page is PRIVATE — the handler gates it on M/TM/A; it adds no authority of its own. *)
let reports_queue_page ?user ?(rail_communities = []) ~(channels : channel list)
    ~(sections : community_section list) ~(community : community) ~(status : Db.report_status)
    ~(reports : Db.report_row list) ~(previews : (int * (string * string)) list) request =
  let esc = Components.html_escape in
  let csrf_token = Dream.csrf_tag request in
  let slug = esc community.slug in
  let reason_label = function
    | Db.Report_spam -> "Spam"
    | Db.Report_abuse -> "Abuse / harassment"
    | Db.Report_off_topic -> "Off-topic"
    | Db.Report_illegal -> "Illegal / dangerous"
    | Db.Report_other -> "Other" in
  let target_label = function
    | Db.Report_post -> "post"
    | Db.Report_comment -> "comment"
    | Db.Report_chat_message -> "chat message" in
  let action_kind_label = function
    | Db.Report_removed_content -> "Content removed"
    | Db.Report_banned_author -> "Author banned"
    | Db.Report_other_action -> "Action taken" in
  (* Reuse the existing badge palette so the queue needs no new status-pill CSS. *)
  let status_badge = function
    | Db.Report_open -> "<span class='cm-badge cm-badge--active'>Open</span>"
    | Db.Report_action_taken -> "<span class='cm-badge cm-badge--top'>Action taken</span>"
    | Db.Report_dismissed -> "<span class='cm-badge cm-badge--archived'>Dismissed</span>" in
  let tab st label =
    let cls = if st = status then "cm-nav-link cm-nav-link--active" else "cm-nav-link" in
    Printf.sprintf "<a href='/c/%s/reports?status=%s' class='%s'>%s</a>"
      slug (Db.report_status_to_string st) cls label in
  let tabs = String.concat "\n" [
    tab Db.Report_open "Open";
    tab Db.Report_action_taken "Action taken";
    tab Db.Report_dismissed "Dismissed";
  ] in
  let excerpt s =
    let t = String.trim s in
    if String.length t <= 140 then t else String.sub t 0 137 ^ "\xe2\x80\xa6" in
  let render_row (r : Db.report_row) =
    let reporter = esc r.reporter_username in
    let author = match r.target_author_username with
      | Some u -> Printf.sprintf "<a href='/u/%s' class='cm-table-actor'>u/%s</a>" (esc u) (esc u)
      | None -> "<span class='cm-table-target'>unknown</span>" in
    let details_html = match r.details with
      | Some d when String.trim d <> "" ->
          Printf.sprintf "<p class='cm-table-reason' style='margin:4px 0 0;'>%s</p>" (esc (excerpt d))
      | _ -> "" in
    (* Context link + live preview come from the handler's bounded enrichment; a row not
       present (chat/deleted/over-cap) degrades rather than 500s. safe_internal_path gates
       the server-built path. *)
    let context_html = match List.assoc_opt r.id previews with
      | Some (url, preview) ->
          Printf.sprintf
            "<a href='%s' class='cm-table-actor'>View %s &rarr;</a><br><span class='cm-table-target ph-mask'>%s</span>"
            (Components.safe_internal_path url) (target_label r.target_type) (esc (excerpt preview))
      | None -> "<span class='cm-table-target'>Target unavailable or deleted</span>" in
    (* Resolution controls live only on still-open reports; the handler re-checks both that
       the report is open and that the actor is M/TM/A (renderer is never the boundary). One
       form, two formaction submits, so the optional note applies to whichever action fires —
       no JS, no modal. Resolved rows show the recorded outcome instead. *)
    let actions_html =
      if r.status = Db.Report_open then
        Printf.sprintf "
          <form method='POST' class='cm-report-actions'>
            %s
            <textarea name='resolution_note' class='cm-report-note' rows='2' maxlength='1000' placeholder='Optional note'></textarea>
            <div class='cm-report-btns'>
              <button type='submit' formaction='/c/%s/reports/%d/dismiss' class='cm-btn-sm'>Dismiss</button>
              <button type='submit' formaction='/c/%s/reports/%d/action' class='cm-btn-sm cm-btn-sm--ok'>Mark action taken</button>
            </div>
          </form>"
          csrf_token slug r.id slug r.id
      else
        let kind = match r.action_kind with
          | Some k -> Printf.sprintf "<div>%s</div>" (esc (action_kind_label k))
          | None -> "" in
        let resolved = match r.resolved_at with
          | Some t -> Printf.sprintf "<div>Resolved %s</div>" (Components.time_ago t)
          | None -> "" in
        let note = match r.resolution_note with
          | Some n when String.trim n <> "" -> Printf.sprintf "<div class='ph-mask'>Note: %s</div>" (esc (excerpt n))
          | _ -> "" in
        let body = kind ^ resolved ^ note in
        if body = "" then "<span class='cm-table-target'>&mdash;</span>"
        else Printf.sprintf "<div class='cm-report-meta'>%s</div>" body
    in
    Printf.sprintf "
      <tr>
        <td class='cm-table-when'>#%d<br>%s<br>%s</td>
        <td><a href='/u/%s' class='cm-table-actor'>u/%s</a></td>
        <td><span class='cm-badge'>%s</span> &middot; %s<br><span class='cm-table-target'>by %s</span>%s</td>
        <td>%s</td>
        <td>%s</td>
      </tr>"
      r.id (Components.time_ago r.created_at) (status_badge r.status)
      reporter reporter
      (target_label r.target_type) (esc (reason_label r.reason)) author details_html
      context_html
      actions_html
  in
  let empty_msg = match status with
    | Db.Report_open -> "No open reports. Nothing needs your attention right now."
    | Db.Report_action_taken -> "No reports have been actioned yet."
    | Db.Report_dismissed -> "No reports have been dismissed yet." in
  let table_body =
    if reports = [] then
      Printf.sprintf "<tr><td colspan='5' class='cm-table-empty'>%s</td></tr>" empty_msg
    else String.concat "\n" (List.map render_row reports) in
  (* The per-row preview lookup is bounded (see handler). If more reports exist than were
     enriched, say so rather than silently showing "unavailable" for the tail. *)
  let preview_note =
    if List.length reports > List.length previews
       && List.exists (fun (r : Db.report_row) -> not (List.mem_assoc r.id previews)) reports
    then "<p class='cm-panel-desc'>Context previews are shown for the most recent reports; older rows link from their target type where available.</p>"
    else "" in
  let content = Printf.sprintf "
    <div class='cm-wrap cm-wrap--wide'>
        <div class='cm-head'>
            <h1 class='cm-h1'>Reports <span class='accent'>queue</span></h1>
            <span class='mono launch-reports-ctx'>/c/%s</span>
            <a href='/c/%s/settings?panel=moderation' class='cm-back'>&larr; Back to settings</a>
        </div>
        <section class='cm-panel'>
            <h2 class='cm-panel-title'>Member reports</h2>
            <p class='cm-panel-desc'>Content flagged by members of this community, newest first. Private to moderators.</p>
            <nav class='cm-nav'>%s</nav>
            %s
            <div class='cm-table-wrap'>
                <table class='cm-table'>
                    <thead>
                        <tr>
                            <th>Report</th>
                            <th>Reporter</th>
                            <th>Reason / target</th>
                            <th>Context</th>
                            <th>Resolution</th>
                        </tr>
                    </thead>
                    <tbody>%s</tbody>
                </table>
            </div>
        </section>
    </div>"
    slug
    slug
    tabs
    preview_note
    table_body
  in
  (* Cartographic launch shell (pass 14B): the same four-pane chrome as the
     sibling community routes, wrapping the review docket verbatim — the cm-*
     report rows, the one-form-two-formaction dismiss/action submits, the
     status tabs and the ph-mask replay classes are treated as pinned, so only
     this outer document and the additive mono /c/:slug context above changed.
     The sidebar reuses the shared knowledge grammar with Reports active;
     can_manage is true by construction here — the handler only renders this
     page for M/TM/A viewers, exactly the settings gate. [rail_communities]
     carries the viewer's joined communities as the handler loaded them
     (post-authorization only); ordering, dedup by slug and the single active
     marker stay owned by the shared launch doc builder. Replay privacy keeps
     the existing inner ph-no-capture guard for private communities (the
     launch shell additionally marks .shell, like the sibling routes). *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ~reports_active:true ~can_manage:true ()
  in
  Components.launch_community_page ?user ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-reports"
    ~title:(community.name ^ " — Reports")
    ~content:(Components.private_replay_guard ~community content) ()

let post_page ?user ?(noindex=false) ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community ~user_communities:_ ~moderated_communities:_ user_post_votes user_comment_votes (post : post) (comments : comment list) request =
  let csrf_token = Dream.csrf_tag request in
  let current_user = Dream.session_field request "username" in

  (* Recursive comment tree: children filtered at render time rather than
     pre-grouped in SQL to keep the query simple and avoid a recursive CTE. *)
  let rec render_comment_tree all_comments current_parent_id =
    let children = List.filter (fun (c : comment) -> c.parent_id = current_parent_id) all_comments in

    if children = [] then ""
    else
      let children_html = List.map (fun (c : comment) ->
        let nested_html = render_comment_tree all_comments (Some c.id) in

        (* Reply button toggles a hidden form; splitting button from form keeps the
           action bar flex container clean — form spans full width below the bar. *)
        let reply_button =
          if is_member then
            Printf.sprintf "<button type='button' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\" class='flex items-center gap-1 text-xs font-bold text-gray-500 hover:text-gray-900 bg-transparent'>💬 Reply</button>"
              c.id
          else ""
        in
        let reply_form_html =
          if is_member then
            Printf.sprintf "
            <form id='reply-form-%d' action='/comments' method='POST' class='hidden w-full mt-3 mb-2'>
                %s
                <input type='hidden' name='post_id' value='%d'>
                <input type='hidden' name='parent_id' value='%d'>
                <textarea name='content' required rows='3' class='w-full p-3 border border-[#E0D9CC] rounded-xl shadow-sm focus:outline-none focus:ring-1 focus:ring-[#C94C4C] focus:border-[#C94C4C] text-sm' placeholder='Write a reply...'></textarea>
                <div class='flex justify-end gap-2 mt-2'>
                    <button type='button' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\" class='text-sm text-gray-500 font-medium hover:text-gray-700 px-3 py-1.5'>Cancel</button>
                    <button type='submit' class='bg-[#C94C4C] text-white text-sm font-medium px-4 py-1.5 rounded-full hover:bg-[#A83A3A] transition'>Post Reply</button>
                </div>
            </form>" c.id csrf_token post.id c.id c.id
          else ""
        in

        let current_vote = Option.value ~default:0 (List.assoc_opt c.id user_comment_votes) in

        let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
        let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in

        let up_action = if current_vote = 1 then 0 else 1 in
        let down_action = if current_vote = -1 then 0 else -1 in

        let is_admin = Dream.session_field request "is_admin" = Some "true" in
        (* Tombstone sentinels written by soft_delete_comment / admin_delete_comment. *)
        let is_comment_deleted =
          Components.is_deleted_user c.username
          || c.content = "[deleted]"
          || c.content = "[removed by admin]"
          || c.content = "[removed by moderator]"
        in
        let comment_target_is_admin = List.mem c.username admin_usernames in
        (* Rule A/B/C: strictly mutually exclusive — mirrors post action_btn logic.
           Rule A: own comment → personal Delete. Rule B: mod (not own, not admin target) → Mod Remove dialog.
           Rule C: admin acting without mod role (not own, not admin target) → Admin Remove dialog. *)
        let delete_comment_btn =
          if is_comment_deleted then ""
          else match current_user with
          | None -> ""
          | Some u ->
              if u = c.username then
                (* Rule A: personal delete — no audit trail needed *)
                Printf.sprintf "<form action='/delete-comment' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this comment? This action cannot be undone.')\">
                    %s <input type='hidden' name='comment_id' value='%d'>
                    <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold'>🗑️</button>
                </form>" csrf_token c.id
              else if is_current_user_mod && not comment_target_is_admin then
                (* Rule B: mod removal — dialog enforces a public reason in the mod log *)
                Printf.sprintf "
                  <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🛡️ Mod Remove</button>
                  <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-amber-50 border-b border-amber-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-amber-900'>🛡️ Moderator Removal</h3>
                        <p class='text-xs text-amber-700 mt-0.5'>This action is logged publicly in the mod log.</p>
                      </div>
                      <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required maxlength='255' rows='4'
                            placeholder='Explain why this comment is being removed (visible to the community)...'
                            class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id post.community_slug c.id csrf_token c.id
              else if is_admin && not comment_target_is_admin then
                (* Rule C: admin override — logged as admin_delete_comment in mod_actions *)
                Printf.sprintf "
                  <button onclick=\"document.getElementById('mod-modal-comment-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Remove</button>
                  <dialog id='mod-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
                    <div class='bg-white rounded-2xl overflow-hidden'>
                      <div class='bg-red-50 border-b border-red-200 px-6 py-4'>
                        <h3 class='text-base font-bold text-red-900'>⚡ Admin Intervention</h3>
                        <p class='text-xs text-red-700 mt-0.5'>This action is logged publicly as an admin override.</p>
                      </div>
                      <form action='/c/%s/comments/%d/mod_delete' method='POST' class='px-6 py-5 flex flex-col gap-4'>
                        %s
                        <label class='flex flex-col gap-1.5'>
                          <span class='text-sm font-semibold text-gray-700'>Reason <span class='text-red-500'>*</span></span>
                          <textarea name='reason' required maxlength='255' rows='4'
                            placeholder='Explain the admin intervention reason (visible to the community)...'
                            class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                        </label>
                        <div class='flex justify-end gap-2 pt-1'>
                          <button type='button' onclick=\"document.getElementById('mod-modal-comment-%d').close()\"
                            class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                          <button type='submit'
                            class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Removal</button>
                        </div>
                      </form>
                    </div>
                  </dialog>"
                  c.id c.id post.community_slug c.id csrf_token c.id
              else ""
        in

        (* Ban button mirrors render_post's ban_btn logic; closed over post.community_id.
           banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
           Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority. *)
        let ban_comment_btn =
          if (is_current_user_mod || is_admin) && not (comment_target_is_admin && not is_admin) then
            match current_user with
            | Some u when u <> c.username && not (Components.is_deleted_user c.username) ->
                if List.mem c.username banned_usernames then
                  "<span class='text-xs text-red-600 font-bold'>🚫 Banned</span>"
                else if is_current_user_mod then
                  (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
                  Printf.sprintf "
                    <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors'>🔨 Mod Ban</button>
                    <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
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
                              class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                          </label>
                          <div class='flex justify-end gap-2 pt-1'>
                            <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                              class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                            <button type='submit'
                              class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                          </div>
                        </form>
                      </div>
                    </dialog>"
                    c.id c.id csrf_token c.username post.community_id c.id
                else
                  (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
                  Printf.sprintf "
                    <button onclick=\"document.getElementById('ban-modal-comment-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors'>⚡ Admin Ban</button>
                    <dialog id='ban-modal-comment-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
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
                              class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                          </label>
                          <div class='flex justify-end gap-2 pt-1'>
                            <button type='button' onclick=\"document.getElementById('ban-modal-comment-%d').close()\"
                              class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                            <button type='submit'
                              class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                          </div>
                        </form>
                      </div>
                    </dialog>"
                    c.id c.id csrf_token c.username post.community_id c.id
            | _ -> ""
          else ""
        in

        (* safe_img_src (via user_avatar) replaces the prior raw interpolation of the stored
           avatar_url; same markup, same letter-tile fallback. *)
        let avatar_html =
          Components.user_avatar ~alt:"Avatar"
            ~img_class:"w-7 h-7 rounded-full object-cover shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] flex-shrink-0"
            ~tile_class:"w-7 h-7 bg-[#DFF5F8] rounded-full flex items-center justify-center text-xs text-[#C94C4C] font-bold shadow-sm border border-[#A8DDE8] flex-shrink-0"
            ~username:c.username c.avatar_url
        in

        let upvote_html = match current_user with
          | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-xs font-bold leading-none'>▲</button></form>" csrf_token c.id up_action up_color
          | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 text-xs font-bold leading-none'>▲</a>"
        in
        let downvote_html =
          if not post.allow_downvotes then ""
          else match current_user with
          | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-xs font-bold leading-none'>▼</button></form>" csrf_token c.id down_action down_color
          | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] text-xs font-bold leading-none'>▼</a>"
        in

        let op_badge =
          if c.username = post.username then
            "<span class='ml-1.5 font-bold text-[10px] bg-[#DFF5F8] text-[#69C3D2] px-1.5 py-0.5 rounded'>OP</span>"
          else ""
        in

        (* Separate IDs for content and children so toggleComment can collapse each independently. *)
        let toggle_btn = Printf.sprintf
          "<button type='button' onclick='toggleComment(%d, this)' class='text-xs text-gray-400 hover:text-gray-700 font-mono transition-colors'>[-]</button>"
          c.id
        in

        (* comment-children omitted when empty so toggleComment null-guards cleanly.
           ml-2.5 aligns the thread line under the avatar (w-5 = 1.25rem = ml-2.5 + half border). *)
        let comment_children_div =
          if nested_html = "" then ""
          else Printf.sprintf
            "<div id='comment-children-%d' class='pl-3 border-l-2 border-[#E0D9CC] ml-2.5 mt-2'>%s</div>"
            c.id nested_html
        in

        let total_comment_contribs = c.author_local_post_count + c.author_local_comment_count in
        let comment_active_since = match c.author_first_active_at with
          | None | Some "" -> ""
          | Some ts -> Printf.sprintf " · active since %s" (Components.format_month_year ts)
        in
        let comment_local_stats = Printf.sprintf "<span class='font-semibold text-red-700'>%d</span> local karma · <span class='font-semibold text-red-700'>%d</span> contributions here%s"
          c.author_local_karma total_comment_contribs comment_active_since
        in
        Printf.sprintf "
        <div class='mt-3 hover:bg-[#EDE9DF] transition rounded-r pr-2 py-1'>
            <div class='flex items-start gap-2 mb-1'>
                %s
                %s
                <div class='min-w-0 flex-1'>
                    <div class='flex flex-wrap items-center gap-1 text-xs text-gray-500'>
                        %s%s
                        <span class='text-gray-400'>•</span>
                        <span class='text-gray-400'>%s</span>
                        %s
                        %s
                    </div>
                    <div class='text-xs text-gray-400 mt-0.5'>%s</div>
                </div>
            </div>
            <div id='comment-content-%d'>
                <div class='text-sm text-gray-900 whitespace-pre-wrap break-words'>%s</div>
                <div class='flex items-center gap-3 mt-2'>
                    <div class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-2 py-0.5'>
                        %s
                        <span class='text-xs font-semibold text-gray-700'>%d</span>
                        %s
                    </div>
                    %s
                </div>
                %s
            </div>
            %s
        </div>"
        toggle_btn
        avatar_html (Components.render_author ~mod_usernames ~admin_usernames c.username) op_badge
        (Components.time_ago c.created_at)
        delete_comment_btn ban_comment_btn
        comment_local_stats
        c.id (Components.html_escape c.content)
        upvote_html c.score downvote_html
        reply_button
        reply_form_html
        comment_children_div
      ) children in
      String.concat "\n" children_html
  in

  let comments_html =
    if comments = [] then "<p class='text-gray-500 italic mt-4'>No comments yet.</p>"
    else render_comment_tree comments None
  in

  let action_section =
    if is_member then
      Printf.sprintf "
      <form action='/comments' method='POST' class='mt-0'>
          %s
          <input type='hidden' name='post_id' value='%d'>
          <textarea name='content' required rows='2' class='w-full border border-[#E0D9CC] rounded-xl px-4 py-3 text-sm bg-gray-50 focus:bg-white focus:ring-1 focus:ring-[#C94C4C] focus:border-[#C94C4C] resize-y transition-colors placeholder-gray-400' placeholder='Add a comment...'></textarea>
          <div class='flex justify-end mt-2 mb-8'>
              <button type='submit' class='px-4 py-1.5 text-sm font-medium bg-[#C94C4C] text-white rounded-full hover:bg-[#A83A3A] transition-colors shadow-sm'>Comment</button>
          </div>
      </form>" csrf_token post.id
    else
      Printf.sprintf "
      <div class='mt-6 mb-8 p-6 bg-[#F0EDE4] rounded-xl border border-[#E8E2D9] text-center'>
          <h3 class='text-gray-900 font-bold mb-2'>Join the discussion</h3>
          <p class='text-[#69C3D2] text-sm mb-4'>You must be a member of /c/%s to comment.</p>
          <form action='/join' method='POST'>
              %s
              <input type='hidden' name='community_id' value='%d'>
              <input type='hidden' name='redirect_to' value='/p/%d'>
              <button type='submit' class='bg-[#C94C4C] text-white px-6 py-2 rounded-full font-bold hover:bg-[#A83A3A] transition shadow-sm'>Join /c/%s</button>
          </form>
      </div>" post.community_slug csrf_token post.community_id post.id post.community_slug
  in

  let post_content = match post.content with | Some c -> Printf.sprintf "<div class='ph-mask text-sm text-gray-800 leading-relaxed whitespace-pre-wrap break-words mt-2 mb-3'>%s</div>" (Components.html_escape c) | None -> "" in
  let link_content = match post.url with | Some u -> Printf.sprintf "<div class='mb-6'><a href='%s' target='_blank' class='text-blue-600 hover:underline break-all'>🔗 %s</a></div>" (Components.safe_url u) (Components.html_escape u) | None -> "" in
  (* Image stored as /static/uploads/<uuid>.webp — served directly by Dream.static. *)
  let image_content = match post.image_url with
    | None -> ""
    | Some img -> Printf.sprintf "<div class='mb-4'><img src='%s' alt='Post image' class='w-full max-h-[700px] object-contain bg-stone-900 rounded-xl border border-[#E0D9CC]'></div>"
        (Components.html_escape img)
  in

  let current_vote_direction = match List.assoc_opt post.id user_post_votes with Some d -> d | None -> 0 in
  let up_color = if current_vote_direction = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote_direction = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
  let up_action = if current_vote_direction = 1 then 0 else 1 in
  let down_action = if current_vote_direction = -1 then 0 else -1 in
  (* Voting pill mirrors the pattern in components.ml render_post: toggle by sending
     direction=0 when already voted, avoiding a separate undo endpoint.
     Comments and Share pills share the same action bar to keep post metadata
     actions cohesive and avoid redundant top-of-post share button placement. *)
  let comments_pill =
    Printf.sprintf "<div class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5 text-sm font-medium text-gray-700'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z'/></svg>%d</div>"
      post.comment_count
  in
  let share_pill =
    Printf.sprintf "<button type='button' onclick='copyPostLink(\"/p/%d\", this)' class='flex items-center gap-1.5 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5 text-sm font-medium text-gray-700 hover:bg-gray-100 transition-colors cursor-pointer'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z'/></svg>Share</button>"
      post.id
  in
  let voting_pill =
    let downvote_btn_logged_in =
      if post.allow_downvotes then
        Printf.sprintf "<form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-sm font-bold leading-none'>▼</button></form>"
          csrf_token post.id down_action down_color
      else ""
    in
    let downvote_btn_logged_out =
      if post.allow_downvotes then "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] text-sm font-bold leading-none'>▼</a>"
      else ""
    in
    match current_user with
    | Some _ ->
        Printf.sprintf "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5'><form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-sm font-bold leading-none'>▲</button></form><span class='text-sm font-semibold text-gray-700'>%d</span>%s</div>%s%s</div>"
          csrf_token post.id up_action up_color post.score downvote_btn_logged_in comments_pill share_pill
    | None ->
        Printf.sprintf "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-[#E0D9CC] rounded-full px-3 py-1.5'><a href='/login' class='text-gray-400 hover:text-orange-500 text-sm font-bold leading-none'>▲</a><span class='text-sm font-semibold text-gray-700'>%d</span>%s</div>%s%s</div>"
          post.score downvote_btn_logged_out comments_pill share_pill
  in

  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  (* Sentinels match soft_delete_post / admin_delete_post exactly.
     post.content is string option — None means a link post with no body (not deleted). *)
  let is_post_deleted =
    (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_")
    || post.content = Some "[deleted]"
    || post.content = Some "[removed by admin]"
    || post.content = Some "[removed by moderator]"
  in
  let post_target_is_admin = List.mem post.username admin_usernames in
  (* Rule A/B/C: strictly mutually exclusive — prevents admins silently using the personal
     Delete path to avoid the public mod log (admin spoofing). No two rules fire at once.
     Rule A: own post → personal Delete. Rule B: mod/top_mod (not own post) → Mod Remove dialog.
     Rule C: admin acting without mod role (not own post) → Admin Remove dialog. *)
  let post_action_btn =
    if is_post_deleted then ""
    else match current_user with
    | None -> ""
    | Some u ->
        if u = post.username then
          (* Rule A: personal delete — no audit required *)
          Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0 ml-3' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
              %s <input type='hidden' name='post_id' value='%d'>
              <button type='submit' class='text-sm text-red-500 hover:text-red-700 font-bold'>🗑️ Delete</button>
          </form>" csrf_token post.id
        else if is_current_user_mod && not post_target_is_admin then
          (* Rule B: mod/top_mod removal — dialog enforces a public reason *)
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-1 transition-colors ml-3'>🛡️ Mod Remove</button>
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
                      class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
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
        else if is_admin && not post_target_is_admin then
          (* Rule C: admin override (not a mod of this community) — logged as admin_delete_post *)
          Printf.sprintf "
            <button onclick=\"document.getElementById('mod-modal-%d').showModal()\" class='text-xs font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-1 transition-colors ml-3'>⚡ Admin Remove</button>
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
                      class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
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

  (* post_page renders the post inline, not via Components.render_post, so ban_post_btn
     must be computed here separately — same guard logic as components.ml's ban_btn.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Rule B (mod) and Rule C (admin-only) are mutually exclusive — mod role takes priority. *)
  let ban_post_btn =
    if (is_current_user_mod || is_admin) && not (post_target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username
          && not (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_") ->
          if List.mem post.username banned_usernames then
            "<span class='text-sm text-red-600 font-bold ml-1'>🚫 Banned</span>"
          else if is_current_user_mod then
            (* Rule B: mod/top_mod ban — dialog enforces a public reason *)
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='text-sm font-bold text-amber-700 hover:text-amber-900 border border-amber-300 bg-amber-50 hover:bg-amber-100 rounded px-2 py-0.5 transition-colors ml-1'>🔨 Mod Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
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
                        class='w-full rounded-xl border border-amber-300 bg-amber-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-amber-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-amber-600 hover:bg-amber-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (Components.html_escape post.username) post.community_id post.id
          else
            (* Rule C: admin acting without mod role — handler prefixes reason as admin override *)
            Printf.sprintf "
              <button onclick=\"document.getElementById('ban-modal-postpage-%d').showModal()\" class='text-sm font-bold text-red-700 hover:text-red-900 border border-red-300 bg-red-50 hover:bg-red-100 rounded px-2 py-0.5 transition-colors ml-1'>⚡ Admin Ban</button>
              <dialog id='ban-modal-postpage-%d' class='rounded-2xl shadow-2xl p-0 w-full max-w-md backdrop:bg-black/60 backdrop:backdrop-blur-sm border-0'>
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
                        class='w-full rounded-xl border border-red-300 bg-red-50 px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-red-400 resize-none'></textarea>
                    </label>
                    <div class='flex justify-end gap-2 pt-1'>
                      <button type='button' onclick=\"document.getElementById('ban-modal-postpage-%d').close()\"
                        class='px-4 py-2 rounded-xl text-sm font-semibold text-gray-600 bg-gray-100 hover:bg-gray-200 transition-colors'>Cancel</button>
                      <button type='submit'
                        class='px-4 py-2 rounded-xl text-sm font-bold text-white bg-red-600 hover:bg-red-700 transition-colors shadow-sm'>Confirm Ban</button>
                    </div>
                  </form>
                </div>
              </dialog>"
              post.id post.id csrf_token (Components.html_escape post.username) post.community_id post.id
      | _ -> ""
    else ""
  in

  let post_rules_html =
    match community.rules with
    | Some rules when rules <> "" ->
        Printf.sprintf "<div class='mt-4 pt-4 border-t border-gray-100'><h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-2'>Rules</h3><p class='text-xs text-gray-600 whitespace-pre-wrap'>%s</p></div>" (Components.html_escape rules)
    | _ -> ""
  in

  let post_mods_html =
    if mod_usernames = [] then "<p class='text-xs text-gray-400 italic'>No moderators yet.</p>"
    else
      let links = String.concat "\n" (List.map (fun u ->
        Printf.sprintf "<li><a href='/u/%s' class='text-sm text-gray-700 hover:text-[#C94C4C] transition'>u/%s</a></li>" (Components.html_escape u) (Components.html_escape u)
      ) mod_usernames) in
      Printf.sprintf "<ul class='space-y-1'>%s</ul>" links
  in

  let post_right_sidebar = Printf.sprintf "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h2 class='font-bold text-gray-900 mb-1'>%s</h2>
        <div class='text-xs text-[#C94C4C] font-mono mb-3'>/c/%s</div>
        <p class='text-sm text-gray-600'>%s</p>
        %s
        <a href='/new-post?community=%s' class='bg-[#C94C4C] text-white rounded-xl px-4 py-2 w-full block text-center mt-4 hover:bg-[#A83A3A] transition text-sm font-semibold'>+ Create Post</a>
        <a href='/c/%s/modlog' class='mt-2 flex items-center gap-2 text-sm text-gray-400 hover:text-gray-600 transition-colors'>
            <span>&#128220;</span><span>Public Modlog</span>
        </a>
        <div class='mt-4 pt-4 border-t border-gray-100'>
            <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-2'>Moderators</h3>
            %s
        </div>
    </div>"
    (Components.html_escape community.name) (Components.html_escape community.slug)
    (Components.html_escape (Option.value ~default:"No description." community.description))
    post_rules_html (Components.html_escape community.slug) (Components.html_escape community.slug) post_mods_html
  in

  let post_author_initial =
    if is_post_deleted then "?"
    else String.uppercase_ascii (String.sub post.username 0 1)
  in
  let post_local_stats_html =
    let total_contribs = post.author_local_post_count + post.author_local_comment_count in
    let active_since_part = match post.author_first_active_at with
      | None | Some "" -> ""
      | Some ts -> Printf.sprintf " · active since %s" (Components.format_month_year ts)
    in
    Printf.sprintf "<span class='font-semibold text-red-700'>%d</span> local karma · <span class='font-semibold text-red-700'>%d</span> contributions here%s"
      post.author_local_karma total_contribs active_since_part
  in
  let content = Printf.sprintf "
    <div class='flex flex-col lg:flex-row gap-6 items-start xl:translate-x-20 2xl:translate-x-24'>
        <div class='min-w-0 flex-1'>
            <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-6 mb-6'>
                <div class='mb-4'>
                    <div class='flex items-center gap-1 flex-wrap text-sm mb-3'>
                        <a href='/c/%s' class='font-bold text-[#C94C4C] hover:underline'>/c/%s</a>
                        %s
                    </div>
                    <div class='flex items-start gap-3 mb-3'>
                        <div class='w-8 h-8 bg-[#DFF5F8] rounded-full flex items-center justify-center text-sm text-[#C94C4C] font-bold border border-[#A8DDE8] flex-shrink-0'>%s</div>
                        <div class='min-w-0 flex-1'>
                            <div class='flex flex-wrap items-center gap-1.5 text-sm text-gray-700'>
                                %s
                                <span class='text-gray-400'>·</span>
                                <span class='text-gray-500'>%s</span>
                                %s
                            </div>
                            <div class='text-xs text-gray-500 mt-0.5'>%s</div>
                        </div>
                    </div>
                    <h1 class='text-lg md:text-xl font-bold text-gray-900 mb-3 leading-snug'>%s</h1>
                </div>
                %s
                %s
                %s
                %s
                %s
                <div>%s</div>
            </div>
        </div>
        <div class='w-80 hidden lg:flex flex-col gap-6 self-start sticky top-20 h-[calc(100vh-5rem)] overflow-y-auto pb-8 [&::-webkit-scrollbar]:w-1.5 [&::-webkit-scrollbar-track]:bg-transparent [&::-webkit-scrollbar-thumb]:bg-transparent hover:[&::-webkit-scrollbar-thumb]:bg-gray-300 [&::-webkit-scrollbar-thumb]:rounded-full'>
            %s
        </div>
    </div>"
    post.community_slug post.community_slug
    (match post.section_name, post.section_slug with
     | Some sn, Some ss ->
         Printf.sprintf "<span class='text-gray-400 mx-1'>›</span><a href='/c/%s/s/%s' class='font-bold text-[#C94C4C] hover:underline'>%s</a>"
           (Components.html_escape post.community_slug) (Components.html_escape ss) (Components.html_escape sn)
     | _ when post.community_sections_enabled ->
         Printf.sprintf "<span class='text-gray-400 mx-1'>›</span><a href='/c/%s/s/uncategorized' class='font-bold text-[#C94C4C] hover:underline'>Uncategorized</a>"
           (Components.html_escape post.community_slug)
     | _ -> "")
    post_author_initial
    (Components.render_author ~mod_usernames ~admin_usernames post.username) (Components.time_ago post.created_at) (post_action_btn ^ ban_post_btn)
    post_local_stats_html
    post.title
    image_content link_content post_content voting_pill
    action_section comments_html
    post_right_sidebar
  in
  (* Inline script keeps post_page self-contained; prepended so the function
     is defined before any onclick fires (no DOMContentLoaded needed). *)
  let toggle_script = {|<script>
function toggleComment(id, btn) {
  const content = document.getElementById('comment-content-' + id);
  const children = document.getElementById('comment-children-' + id);
  const isCollapsed = content.classList.contains('hidden');
  if (isCollapsed) {
    content.classList.remove('hidden');
    if (children) children.classList.remove('hidden');
    btn.innerText = '[-]';
  } else {
    content.classList.add('hidden');
    if (children) children.classList.add('hidden');
    btn.innerText = '[+]';
  }
}
</script>|} in
  Components.layout ?user ~noindex ~request ~analytics_community:(post.community_id, community.visibility)
    ~title:post.title (toggle_script ^ content)

(* === USER === *)

(* /u/:username — the public user profile, on the launch app chrome
   (Components.launch_app_page: earde.css only, no shell.css / account.css).
   The inner account-* markup (account-prof header, account-badges,
   account-bio, account-tabs, account-thread rows with the /vote form DOM the
   shared behavior script drives, account-comment, account-comm-card,
   account-admin) is kept and skinned by the "public user profile only"
   integration section at the end of earde.css. .account-bio stays because it
   is a replay-masking selector in analytics.js. rail_communities feeds the
   launch rail only (VIEWER membership, same order as every other launch
   surface); it never enters profile content. Tabs, ?tab= values, default and
   unknown-value fallback, activity ordering and every destination link are
   unchanged. *)
let user_profile_page ?user ?(rail_communities = []) ~is_admin ~is_globally_banned ~profile_id ~admin_usernames ~moderated_communities ~active_tab user_votes username joined_at bio_opt avatar_url_opt karma posts user_comments community_stats request =
  let csrf_token = Dream.csrf_tag request in
  let bio = Option.value ~default:"This user hasn't written a bio yet." bio_opt in
  (* Profile avatar: route the stored URL through Components.user_avatar (safe_img_src) instead
     of raw interpolation. Avatarless profiles now show a local letter tile rather than the old
     external Gravatar "mystery person" default — one fewer third-party image dependency. *)
  let avatar_html =
    Components.user_avatar ~alt:(username ^ " avatar")
      ~img_class:"account-avatar"
      ~tile_class:"account-avatar flex items-center justify-center text-2xl font-bold"
      ~username avatar_url_opt
  in

  (* Profile role badges: ADMIN badge driven by admin_usernames (already fetched),
     MOD badges iterate moderated_communities — one badge per community the user moderates. *)
  let profile_admin_badge =
    if List.mem username admin_usernames then
      "<span class='account-badge account-badge--admin'>Admin</span>"
    else ""
  in
  let mod_badges =
    String.concat " " (List.map (fun (a : community) ->
      Printf.sprintf "<a href='/c/%s' class='account-badge account-badge--mod'>Mod of /c/%s</a>"
        a.slug a.slug
    ) moderated_communities)
  in
  (* Banned badge: shown on the profile header so any visitor sees the account status.
     Admin controls: Ban ↔ Unban toggle driven by is_globally_banned to prevent
     double-ban confusion and surface the current state clearly. *)
  let globally_banned_badge =
    if is_globally_banned then
      "<span class='account-badge account-badge--banned'>Globally banned</span>"
    else ""
  in
  let role_badges =
    if profile_admin_badge = "" && mod_badges = "" && globally_banned_badge = "" then ""
    else Printf.sprintf "<div class='account-badges'>%s%s%s</div>" profile_admin_badge mod_badges globally_banned_badge
  in
  (* Edit Profile: only render when the logged-in user is viewing their own profile.
     Avoids exposing /settings entry point on others' profiles — a cosmetic boundary
     that reinforces the expectation that settings are personal. *)
  let edit_profile_btn =
    match user with
    | Some logged_in when logged_in = username ->
      "<a href='/settings' class='account-btn account-btn--secondary'>Edit profile</a>"
    | _ -> ""
  in
  (* Admin Panel: only render for the logged-in admin on their own profile.
     Gating on both own-profile AND is_admin prevents leaking the /admin route
     to non-admins even if they inspect another admin's profile page. *)
  let admin_panel_btn =
    match user with
    | Some logged_in when logged_in = username && is_admin ->
      "<a href='/admin' class='account-btn account-btn--secondary'>Admin panel</a>"
    | _ -> ""
  in
  let header_actions =
    if edit_profile_btn = "" && admin_panel_btn = "" then ""
    else Printf.sprintf "<div class='account-actions'>%s%s</div>" edit_profile_btn admin_panel_btn
  in

  let admin_controls =
    if is_admin && (Option.value ~default:"" user) <> username then
      let ban_or_unban_btn =
        if is_globally_banned then
          Printf.sprintf "
            <form action='/admin/unban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">
                %s
                <button type='submit' class='account-btn account-btn--secondary'>Unban user</button>
            </form>" profile_id username csrf_token
        else
          Printf.sprintf "
            <form action='/admin/ban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Permanently ban u/%s? They will be blocked from logging in and posting.')\">
                %s
                <button type='submit' class='account-btn'>Ban user</button>
            </form>" profile_id username csrf_token
      in
      Printf.sprintf "
        <div class='account-admin'>
            <h3 class='account-admin-title'>Admin controls</h3>
            %s
        </div>" ban_or_unban_btn
    else ""
  in

  let tab_class active =
    if active_tab = active then "account-tab account-tab--active" else "account-tab"
  in
  (* User-facing label is "Threads"; the query param stays ?tab=posts so existing links,
     the handler's tab dispatch, and bookmarks keep working unchanged. *)
  let tab_nav = Printf.sprintf "
    <div class='account-tabs'>
        <a href='/u/%s?tab=posts' class='%s'>Threads</a>
        <a href='/u/%s?tab=comments' class='%s'>Comments</a>
        <a href='/u/%s?tab=communities' class='%s'>Communities</a>
    </div>" username (tab_class "posts") username (tab_class "comments") username (tab_class "communities")
  in

  (* Profile-specific thread row. The shell's render_forum_row markup is styled only under
     .community-shell (shell.css), so it renders unstyled inside the focused .account-shell.
     This row reuses the SAME real post data and the SAME vote-form DOM (up-form, score span,
     down-form, with the optimistic-vote colour classes) so voting behaves identically — only
     the surrounding layout is account.css-scoped. No query / canonical-URL change. *)
  let render_thread_row (post : Db.post) =
    let current_vote = Option.value ~default:0 (List.assoc_opt post.id user_votes) in
    let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
    let down_color = if current_vote = -1 then "text-[#69C3D2]" else "text-gray-400 hover:text-[#69C3D2]" in
    let up_action = if current_vote = 1 then 0 else 1 in
    let down_action = if current_vote = -1 then 0 else -1 in
    let upvote_html = match user with
      | Some _ -> Printf.sprintf "<form action='/vote' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▲</button></form>" csrf_token post.id up_action up_color
      | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 font-bold text-sm leading-none'>▲</a>"
    in
    let downvote_html =
      if not post.allow_downvotes then ""
      else match user with
      | Some _ -> Printf.sprintf "<form action='/vote' method='POST'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s font-bold text-sm leading-none'>▼</button></form>" csrf_token post.id down_action down_color
      | None -> "<a href='/login' class='text-gray-400 hover:text-[#69C3D2] font-bold text-sm leading-none'>▼</a>"
    in
    let domain_html = match post.url with
      | Some u -> (match Components.extract_domain u with
          | Some d -> Printf.sprintf "<a class='account-thread-domain' href='%s' target='_blank' rel='noopener'>%s ↗</a>" (Components.safe_url u) (Components.html_escape d)
          | None -> "")
      | None -> ""
    in
    let preview_html = match post.content with
      | Some c when String.trim c <> "" -> Printf.sprintf "<div class='account-thread-preview'>%s</div>" (Components.html_escape c)
      | _ -> ""
    in
    (* Each row names its origin community/section, since a profile spans communities. *)
    let context_html =
      let section_html = match post.section_name, post.section_slug with
        | Some name, Some slug when String.trim name <> "" ->
            Printf.sprintf "<span class='sec'>§</span> <a href='/c/%s/s/%s'>%s</a> <span class='dot'>·</span> "
              (Components.html_escape post.community_slug) (Components.html_escape slug) (Components.html_escape name)
        | _ -> ""
      in
      Printf.sprintf "<div class='account-thread-ctx'>%s<a href='/c/%s'>/c/%s</a></div>"
        section_html (Components.html_escape post.community_slug) (Components.html_escape post.community_slug)
    in
    let thread_href = Components.canonical_thread_path post.community_slug post.id post.title in
    Printf.sprintf "
        <div class='account-thread'>
            <div class='account-thread-vote'>%s<span class='account-thread-score'>%d</span>%s</div>
            <div class='account-thread-main'>
                %s
                <div class='account-thread-title'><a href='%s'>%s</a></div>
                %s
                <div class='account-thread-meta'>%s<span>by %s</span><span>%s</span><a href='%s'>💬 %d</a></div>
            </div>
        </div>"
      upvote_html post.score downvote_html
      context_html
      thread_href (Components.html_escape post.title)
      preview_html
      domain_html (Components.render_author ~admin_usernames post.username) (Components.time_ago post.created_at) thread_href post.comment_count
  in

  let posts_html =
    if posts = [] then
      "<div class='account-empty'>This user hasn't started any threads yet.</div>"
    else
      Printf.sprintf "<div class='account-threads'>%s</div>"
        (String.concat "\n" (List.map render_thread_row posts))
  in

  let comments_html =
    if user_comments = [] then
      "<div class='account-empty'>This user hasn't commented anything yet.</div>"
    else
      String.concat "\n" (List.map (fun (_id, content, created_at, post_id, post_title, score) ->
        Printf.sprintf "
          <div class='account-comment'>
              <div class='account-comment-meta'>
                  <span>%s</span>
                  <span class='dot'>·</span>
                  <span>%d points</span>
              </div>
              <div class='account-comment-body'>%s</div>
              <a href='/p/%d' class='account-comment-link'>&#8618; Commented on: %s</a>
          </div>" created_at score (Components.html_escape content) post_id (Components.html_escape post_title)
      ) user_comments)
  in

  let communities_html =
    if community_stats = [] then
      "<div class='account-empty'>No community activity yet.</div>"
    else begin
      let cards = String.concat "\n" (List.map (fun (s : Db.community_user_stat) ->
        let total = s.local_post_count + s.local_comment_count in
        let active_since =
          match s.first_active_at with
          | None -> ""
          | Some ts ->
              Printf.sprintf "<div class='account-comm-sub'>active since %s</div>"
                (Components.format_month_year ts)
        in
        Printf.sprintf "
          <a href='/c/%s' class='account-comm-card'>
              <div class='account-comm-name'>%s</div>
              <div class='account-comm-slug'>/c/%s</div>
              <div class='account-comm-stat'><strong>%d</strong> local karma</div>
              <div class='account-comm-stat'><strong>%d</strong> contributions here</div>
              <div class='account-comm-sub'>%d posts &middot; %d comments</div>
              %s
          </a>"
          s.community_slug
          (Components.html_escape s.community_name)
          s.community_slug
          s.local_karma
          total
          s.local_post_count
          s.local_comment_count
          active_since
      ) community_stats) in
      Printf.sprintf "
        <h3 class='account-section-title'>Community reputation</h3>
        <div class='account-comm-grid'>%s</div>" cards
    end
  in

  let feed_html =
    if active_tab = "comments" then comments_html
    else if active_tab = "communities" then communities_html
    else posts_html
  in

  let body = Printf.sprintf "
    <div class='account-wrap'>
        <div class='account-panel'>
            <div class='account-prof'>
                %s
                <div class='account-prof-body'>
                    <h1 class='account-name'>u/%s</h1>
                    %s
                    <div class='account-stats'>
                        <span><strong>%d</strong> karma</span>
                        <span>Joined %s</span>
                    </div>
                    %s
                </div>
            </div>
            <div class='account-bio'>%s</div>
            %s
        </div>

        %s
        %s
    </div>" avatar_html username role_badges karma joined_at header_actions (Components.html_escape bio) admin_controls tab_nav feed_html
  in
  (* Standard launch scroller column around the untouched account-* fragments;
     no noindex — the profile stays a public, crawlable discovery surface. *)
  let content =
    Printf.sprintf "<div class='scroll'><div class='container container--list'>%s</div></div>" body
  in
  Components.launch_app_page ?user ~request ~rail_communities
    ~page_class:"launch-user-profile" ~title:(username ^ "'s Profile") ~content ()

(* /settings — the account-global settings surface, on the launch app chrome
   (Components.launch_app_page: earde.css only, no shell.css / account.css).
   The five account-panel sections (multipart profile form, password form,
   data export, the consent-managed analytics panel, the delete-account
   danger zone) keep their markup verbatim: field names/order, CSRF
   positions, the data-analytics-* hooks analytics.js drives, and the
   confirmModal onsubmit are the load-bearing contracts. Launch chrome
   renders outside them: the legacy in-page Settings/Notifications account
   nav is superseded by the topbar user menu (same real destinations), and
   the serif page head carries the mono account identifier plus the existing
   view-profile link. Panels are skinned by the "account settings only"
   integration section at the end of earde.css.

   The stored bio and avatar URL are user-controlled (bio via this form,
   avatar_url via the browser-supplied existing_avatar_url field) and were
   previously interpolated raw; they are now escaped at this template
   boundary — textarea text context and value='…' attribute context — per
   the store-raw/escape-at-render convention. *)
let settings_page ?user ?(rail_communities = []) bio avatar_url request =
  let csrf_token = Dream.csrf_tag request in
  let current_bio = Components.html_escape (Option.value ~default:"" bio) in
  let current_avatar = Components.html_escape (Option.value ~default:"" avatar_url) in

  (* Read-only preview of the stored avatar above the upload input. Same Components.user_avatar
     gate (safe_img_src) + letter-tile fallback as the profile header, so a missing/unsafe
     avatar shows the local tile rather than a broken image. Does not change the upload form. *)
  let avatar_preview =
    Components.user_avatar ~alt:"Current avatar"
      ~img_class:"account-avatar"
      ~tile_class:"account-avatar flex items-center justify-center text-2xl font-bold"
      ~username:(Option.value ~default:"" user) avatar_url
  in

  let identity_line =
    match user with
    | Some u ->
        Printf.sprintf "<p class='account-ident'>u/%s</p>"
          (Components.html_escape u)
    | None -> ""
  in
  let view_profile_link =
    match user with
    | Some u ->
        Printf.sprintf
          "<a class='account-view-profile' href='/u/%s'>View public profile &#8599;</a>"
          (Components.html_escape u)
    | None -> ""
  in
  let page_head =
    Printf.sprintf
      "<div class='page__head'><div class='page__head-inner page__head-inner--list account-head-launch'>\
       <div><h1 class='page__title page__title--sm'>Account settings</h1>%s</div>\
       %s</div></div>"
      identity_line view_profile_link
  in

  let body = Printf.sprintf "
    <div class='account-wrap account-wrap--narrow'>
        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Profile information</h2>
            <form action='/settings' method='POST' enctype='multipart/form-data' class='account-form'>
                %s
                <div class='account-field'>
                    <label class='account-label'>Avatar <span class='account-label-opt'>(optional)</span></label>
                    <div class='account-avatar-row'>
                        %s
                        <div class='account-avatar-ctl'>
                            <input type='hidden' name='existing_avatar_url' value='%s'>
                            <input type='file' name='avatar_url' accept='image/*' class='account-file'>
                            <span class='account-hint'>PNG or JPG &middot; square images look best</span>
                        </div>
                    </div>
                </div>
                <div class='account-field'>
                    <label class='account-label'>Bio</label>
                    <textarea name='bio' rows='4' class='account-textarea' placeholder='Tell the community a bit about yourself...'>%s</textarea>
                </div>
                <button type='submit' class='account-btn'>Save profile</button>
            </form>
        </div>

        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Change password</h2>
            <form action='/settings/password' method='POST' class='account-form'>
                %s
                <div class='account-field'>
                    <label class='account-label'>Current password</label>
                    <input type='password' name='old_password' required class='account-input'>
                </div>
                <div class='account-field-pair'>
                    <div class='account-field'>
                        <label class='account-label'>New password</label>
                        <input type='password' name='new_password' required minlength='8' class='account-input'>
                    </div>
                    <div class='account-field'>
                        <label class='account-label'>Confirm new password</label>
                        <input type='password' name='confirm_password' required minlength='8' class='account-input'>
                    </div>
                </div>
                <button type='submit' class='account-btn account-btn--secondary'>Update password</button>
            </form>
        </div>

        <div class='account-panel account-panel--card'>
            <h2 class='account-section-title'>Your data</h2>
            <p class='account-section-desc'>Export your account data as JSON.</p>
            <a href='/export-data' class='account-btn account-btn--secondary'>&#8595; Download my data</a>
        </div>

        <div class='account-panel account-panel--card' data-analytics-settings hidden>
            <h2 class='account-section-title'>Analytics</h2>
            <p class='account-section-desc'>Control whether Earde may collect anonymous product-usage analytics (PostHog) in your browser. Nothing is collected without your consent.</p>
            <div class='account-form'>
                <span data-analytics-state class='account-hint'></span>
                <div>
                    <button type='button' data-analytics-accept class='account-btn account-btn--secondary'>Enable analytics</button>
                    <button type='button' data-analytics-refuse class='account-btn account-btn--secondary'>Disable analytics</button>
                </div>
                <span data-analytics-error hidden class='account-hint' style='color:#C94C4C;'>Couldn&#39;t save your choice &mdash; please try again.</span>
            </div>
        </div>

        <div class='account-panel account-danger'>
            <h2 class='account-section-title'>Danger zone</h2>
            <p class='account-section-desc'>Permanently delete your account and personal data. Your posts and comments will remain, but their author will be anonymized as <strong>[deleted]</strong>. This action is irreversible.</p>
            <form action='/delete-account' method='POST' onsubmit=\"confirmModal(event, 'WARNING: Permanently delete your personal data? This action is irreversible.')\">
                %s
                <button type='submit' class='account-btn account-btn--danger'>Delete account</button>
            </form>
        </div>
    </div>"
    csrf_token avatar_preview current_avatar current_bio csrf_token csrf_token
  in
  let content =
    Printf.sprintf
      "%s<div class='scroll'><div class='container container--list'>%s</div></div>"
      page_head body
  in
  Components.launch_app_page ~noindex:true ?user ~request ~rail_communities
    ~page_class:"launch-account-settings" ~title:"Settings" ~content ()

(* /notifications — the account-global notification center, on the launch
   app chrome (Components.launch_app_page: earde.css only, no shell.css /
   account.css). The row renderer below is kept byte-for-byte: its anatomy
   (account-notif / account-notif-icon / account-notif-body / account-notif-msg
   / account-notif-time, the account-notif--unread accent and its cosmetic
   onclick clear) is pinned by the gated Phnt UI suite's substring checks and
   by the replay-masking selector .account-notif-msg in analytics.js. Rows are
   skinned onto the approved flat .notif anatomy by the "notifications only"
   integration section at the end of earde.css. The handoff's filter tabs and
   "mark all read" POST are deliberately absent — no such routes exist; the
   GET itself marks everything read (Db.mark_notifs_read) exactly as before. *)
let notifications_page ?user ?(rail_communities = []) (notifs : Db.notification list) request =
  let render_notif (n : Db.notification) =
    let unread_class = if n.is_read then "" else " account-notif--unread" in
    (* Project-home notifications are structured: no stored prose, so the
       label and destination derive from the durable kind and the joined
       current project/community identity. Names and slugs are escaped at
       this template boundary; destinations are built structurally from
       the joined slugs (never from anything browser-supplied), and each
       destination page enforces its own authorization. The actor is
       deliberately absent from the copy, so a deleted actor renders
       identically. Should a joined identity be missing mid-race (the
       subject FKs cascade), the row degrades to a generic unlinked line
       rather than inventing names. *)
    let project_home_label_link =
      match n.notif_type with
      | "project_home_requested" | "project_home_accepted"
      | "project_home_rejected" | "project_home_removed" -> (
          match (n.project_name, n.project_slug, n.community_name, n.community_slug) with
          | Some project_name, Some project_slug, Some community_name, Some community_slug ->
              let p = Components.html_escape project_name in
              let c = Components.html_escape community_name in
              let label, href =
                match n.notif_type with
                | "project_home_requested" ->
                    ( Printf.sprintf "%s requested %s as its community home" p c,
                      Printf.sprintf "/c/%s/project-home-requests" (Components.html_escape community_slug) )
                | "project_home_accepted" ->
                    ( Printf.sprintf "%s accepted the community-home request for %s" c p,
                      Printf.sprintf "/projects/%s/request-home" (Components.html_escape project_slug) )
                | "project_home_rejected" ->
                    ( Printf.sprintf "%s rejected the community-home request for %s" c p,
                      Printf.sprintf "/projects/%s/request-home" (Components.html_escape project_slug) )
                | _ ->
                    (* Removal is the one kind whose recipients span BOTH
                       sides: the removal store notifies every project
                       steward and every community top moderator (minus the
                       actor). A steward-only destination would 404 for the
                       moderators, so this points at the community's own
                       page — reachable by a top moderator always, and by a
                       steward for exactly the published (public/unlisted)
                       communities a home can be removed from. Its
                       Connected-projects section is also where the removal
                       is actually visible to either side. *)
                    ( Printf.sprintf "%s is no longer connected to %s as its home" p c,
                      Printf.sprintf "/c/%s" (Components.html_escape community_slug) )
              in
              Some (label, Some href)
          | _ -> Some ("A project community-home update", None))
      | _ -> None
    in
    let message = Option.value n.message ~default:"" in
    let icon = match n.notif_type with
      | "mention"    -> "&#64;"   (* @ symbol — avoids mojibake in Printf *)
      | "mod_action" -> "&#9888;" (* ⚠ warning sign *)
      | "project_home_requested" | "project_home_accepted"
      | "project_home_rejected" | "project_home_removed" -> "&#127968;" (* 🏠 *)
      | _ ->
          (* Legacy comment_reply: distinguish post vs comment reply by message suffix. *)
          let len = String.length message in
          if len >= 5 && String.sub message (len - 5) 5 = "post." then "&#128221;" (* 📝 *)
          else "&#128172;" (* 💬 *)
    in
    (* Project-home labels are built above from already-escaped parts;
       legacy prose is escaped here. *)
    let msg_html =
      match project_home_label_link with
      | Some (label, _) -> label
      | None -> Components.html_escape message
    in
    let inner = Printf.sprintf "
        <div class='account-notif-icon'>%s</div>
        <div class='account-notif-body'>
            <div class='account-notif-msg'>%s</div>
            <div class='account-notif-time'>%s</div>
        </div>" icon msg_html (Components.time_ago n.created_at)
    in
    let link =
      match project_home_label_link with
      | Some (_, link) -> link
      | None -> (
          match n.post_id with
          | Some pid -> Some (Printf.sprintf "/p/%d" pid)
          | None -> None)
    in
    (* Notifications without a destination render as non-clickable divs.
       Read state is already persisted server-side on page load (Db.mark_notifs_read);
       the onclick is a purely cosmetic clear of the unread accent on this visit. *)
    match link with
    | Some href ->
        Printf.sprintf "
    <a href='%s' onclick=\"this.classList.remove('account-notif--unread');\" class='account-notif%s'>%s
    </a>" href unread_class inner
    | None ->
        Printf.sprintf "
    <div class='account-notif%s'>%s
    </div>" unread_class inner
  in
  let list_html =
    if notifs = [] then "<div class='account-empty'>No notifications yet.</div>"
    else Printf.sprintf "<div class='account-notifs'>%s</div>" (String.concat "\n" (List.map render_notif notifs))
  in
  (* Launch chrome renders outside the pinned row fragments: serif page head
     (the legacy in-page Settings/Notifications nav is superseded by the
     topbar user menu), then the standard scroller column. The sub line names
     only kinds the backend actually produces — no "promotions". *)
  let page_head =
    "<div class='page__head'><div class='page__head-inner page__head-inner--list'>\
     <h1 class='page__title'>Notifications</h1>\
     <p class='page__sub'>Replies, mentions, moderation decisions and project requests.</p>\
     </div></div>"
  in
  let content =
    Printf.sprintf "%s<div class='scroll'><div class='container container--list'>%s</div></div>"
      page_head list_html
  in
  Components.launch_app_page ~noindex:true ?user ~request ~rail_communities
    ~page_class:"launch-notifications" ~title:"Notifications" ~content ()

(* === SEARCH === *)

(* Launch search surface (Cartographic Civic pass 12B): the same result body the cool-grey
   shell rendered — header form, tabs, sr-* rows, pager, analytics container, all pinned by
   the analytics suites and the replay-masking selectors — re-parented under the shared
   launch app chrome (Components.launch_app_page, body.launch-search) instead of
   Components.search_page. Answers "where was this discussed / which thread / which
   community / did it come from chat?" An empty query renders a local prompt state instead
   of redirecting; the route and q/t/page semantics are unchanged. The visible Threads tab
   keeps the internal tab value "posts". chat_sources is the bounded per-page provenance
   lookup (post_id -> channel slug/name/source count) from Db.get_thread_sources_for_posts —
   no N+1. rail_communities feeds the launch rail only (viewer membership, same order as
   every other launch surface); it never enters result content. *)
(* user_votes is part of the stable positional API (vote state for the old card renderer); the
   compact search rows show score but no vote arrows, so it is intentionally unused here. *)
let search_results_page ?user ~admin_usernames ?(chat_sources=[]) ?(rail_communities=[]) _user_votes current_page active_tab query (communities: community list) users (posts: post list) comments request =
  (* Escape the raw query once — reused in HTML text, the input value, the <title>, and href
     attributes. HTML-encoding in href is correct: browsers decode entities before navigating. *)
  let q = String.trim query in
  let has_query = q <> "" in
  let eq = Components.html_escape q in
  let csrf_token = Dream.csrf_tag request in
  let et = Components.html_escape active_tab in

  let chat_source_for id =
    List.find_opt (fun (pid, _, _, _) -> pid = id) chat_sources
  in

  (* --- per-tab row renderers (cool-grey .search-shell idiom) --- *)
  let render_community (a: community) =
    (* Reuse the people-result avatar classes (.sr-avatar / --mono) so community and user rows
       align identically; community_avatar falls back to a name letter-tile, never a broken image. *)
    let avatar_html =
      Components.community_avatar ~img_class:"sr-avatar"
        ~tile_class:"sr-avatar sr-avatar--mono" ~name:a.name a.avatar_url
    in
    Printf.sprintf "<a class='sr-row sr-row--link' href='/c/%s'>
        %s
        <div class='sr-row-main'>
          <h3 class='sr-row-title'>%s</h3>
          <div class='sr-row-meta'><span class='sr-c'>/c/%s</span></div>
          <p class='sr-row-excerpt'>%s</p>
        </div>
      </a>"
      (Components.html_escape a.slug) avatar_html (Components.html_escape a.name)
      (Components.html_escape a.slug)
      (Components.html_escape (Option.value ~default:"No description" a.description))
  in

  let render_user (_, username, _, bio, avatar) =
    let eu = Components.html_escape username in
    (* Same safe_img_src gate + letter-tile fallback as the community row above; replaces the
       prior hand-rolled <img>/tile markup (raw url, only html_escape'd) so a stored unsafe
       avatar_url can't render a broken/hostile src. *)
    let avatar_html =
      Components.user_avatar ~img_class:"sr-avatar"
        ~tile_class:"sr-avatar sr-avatar--mono" ~username avatar
    in
    Printf.sprintf "<a class='sr-row sr-row--link sr-row--user' href='/u/%s'>
        %s
        <div class='sr-row-main'>
          <h3 class='sr-row-title'>u/%s</h3>
          <p class='sr-row-excerpt'>%s</p>
        </div>
      </a>"
      eu avatar_html eu (Components.html_escape (Option.value ~default:"" bio))
  in

  let render_search_comment (_, content, username, created_at, post_id, score) =
    Printf.sprintf "<article class='sr-row'>
        <div class='sr-row-main'>
          <div class='sr-row-meta'>by %s <span class='sr-dot'>·</span> %s <span class='sr-dot'>·</span> <span class='sr-score'>%d</span></div>
          <p class='sr-row-excerpt'>%s</p>
          <a class='sr-row-link' href='/p/%d'>Go to thread &rarr;</a>
        </div>
      </article>"
      (Components.render_author ~admin_usernames username) (Components.time_ago created_at)
      score (Components.html_escape content) post_id
  in

  let render_thread (post: post) =
    let section_html = match post.section_name, post.section_slug with
      | Some sn, Some ss ->
          Printf.sprintf " <span class='sr-sep'>&rsaquo;</span> <a class='sr-s' href='/c/%s/s/%s'>%s</a>"
            (Components.html_escape post.community_slug) (Components.html_escape ss) (Components.html_escape sn)
      | _ -> ""
    in
    let source_html = match chat_source_for post.id with
      | None -> ""
      | Some (_, cslug, cname, n) ->
          let count =
            if n > 0 then Printf.sprintf " <span class='sr-dot'>·</span> from %d chat message%s" n (if n = 1 then "" else "s")
            else ""
          in
          Printf.sprintf "<div class='sr-row-src'>Started from <a href='/c/%s/ch/%s'>#%s</a>%s</div>"
            (Components.html_escape post.community_slug) (Components.html_escape cslug)
            (Components.html_escape cname) count
    in
    (* Same admin/mod forms as the feed, kept in a compact secondary strip so they never
       dominate the public results. post_admin_actions returns "" for non-privileged viewers. *)
    let admin = Components.post_admin_actions ~admin_usernames ~csrf_token request post in
    let admin_html = if admin = "" then "" else Printf.sprintf "<div class='sr-row-admin'>%s</div>" admin in
    Printf.sprintf "<article class='sr-row'>
        <div class='sr-row-main'>
          <div class='sr-row-meta'><a class='sr-c' href='/c/%s'>/c/%s</a>%s <span class='sr-dot'>·</span> by %s <span class='sr-dot'>·</span> %s</div>
          <h3 class='sr-row-title'><a href='/p/%d'>%s</a></h3>
          %s
          <div class='sr-row-stats'><span class='sr-score'>%d</span> <span class='sr-dot'>·</span> <a href='/p/%d'>%d comment%s</a></div>
        </div>
        %s
      </article>"
      (Components.html_escape post.community_slug) (Components.html_escape post.community_slug) section_html
      (Components.render_author ~admin_usernames post.username) (Components.time_ago post.created_at)
      post.id (Components.html_escape post.title)
      source_html
      post.score post.id post.comment_count (if post.comment_count = 1 then "" else "s")
      admin_html
  in

  let empty_state msg =
    Printf.sprintf "<div class='sr-empty'>%s</div>" msg
  in

  let content_html, has_next =
    match active_tab with
    | "communities" ->
        if communities = [] then (empty_state (Printf.sprintf "No communities match \"%s\"." eq), false)
        else (String.concat "\n" (List.map render_community communities), List.length communities = 20)
    | "people" ->
        if users = [] then (empty_state (Printf.sprintf "No people match \"%s\"." eq), false)
        else (String.concat "\n" (List.map render_user users), List.length users = 20)
    | "comments" ->
        if comments = [] then (empty_state (Printf.sprintf "No comments match \"%s\"." eq), false)
        else (String.concat "\n" (List.map render_search_comment comments), List.length comments = 20)
    | _ ->
        if posts = [] then (empty_state (Printf.sprintf "No threads match \"%s\"." eq), false)
        else (String.concat "\n" (List.map render_thread posts), List.length posts = 20)
  in

  let tab label tab_value =
    let cls = if tab_value = active_tab then "sr-tab is-active" else "sr-tab" in
    Printf.sprintf "<a class='%s' href='/search?q=%s&t=%s'>%s</a>" cls eq tab_value label
  in
  (* Visible "Threads" maps to the internal tab value "posts" (unchanged route semantics). *)
  let tabs_html = Printf.sprintf "<nav class='sr-tabs'>%s%s%s%s</nav>"
    (tab "Threads" "posts") (tab "Communities" "communities")
    (tab "Comments" "comments") (tab "People" "people")
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
    match Posthog.browser_config () with
    | None -> ""
    | Some _ ->
        let analytics_tab, result_count =
          match active_tab with
          | "communities" -> ("communities", List.length communities)
          | "people" -> ("people", List.length users)
          | "comments" -> ("comments", List.length comments)
          | _ -> ("posts", List.length posts)
        in
        Printf.sprintf
          "<div id='sr-analytics' hidden data-analytics-search-tab='%s' data-analytics-search-result-count='%d' data-analytics-search-page='%d'></div>"
          analytics_tab result_count (max 1 current_page)
  in

  let prev_btn = if current_page <= 1 then "" else
    Printf.sprintf "<a class='sr-page' href='/search?q=%s&t=%s&page=%d'>&larr; Prev</a>" eq et (current_page - 1) in
  let next_btn = if not has_next then "" else
    Printf.sprintf "<a class='sr-page' href='/search?q=%s&t=%s&page=%d'>Next &rarr;</a>" eq et (current_page + 1) in

  (* The search header (label + input) is always present; tabs/results only when a query exists. *)
  let header_html = Printf.sprintf "
    <form class='sr-head' action='/search' method='GET'>
      <label class='sr-label' for='sr-q'>Search</label>
      <div class='sr-inputrow'>
        <span class='sr-sigil'>/</span>
        <input id='sr-q' class='sr-input' type='text' name='q' value='%s' placeholder='grep public threads · communities · people…' autocomplete='off' autofocus>
        <input type='hidden' name='t' value='%s'>
        <button class='sr-go' type='submit'>search</button>
      </div>
    </form>" eq et
  in

  let body =
    if not has_query then
      Printf.sprintf "<div class='sr-wrap'>%s<div class='sr-empty sr-empty--prompt'>Search Earde's public archive — threads, communities, comments and people.</div></div>" header_html
    else
      Printf.sprintf "<div class='sr-wrap'>
          %s
          %s
          <div class='sr-results'>%s</div>
          <div class='sr-pager'>%s%s</div>
          %s
        </div>"
        header_html tabs_html content_html prev_btn next_btn analytics_meta_html
  in
  (* Generic on purpose: the document <title> leaks into analytics surfaces
     (replay snapshots, $title) — the search term must never appear there.
     The visible UI still echoes the query via the input value (masked). *)
  let title = "Search" in
  (* Standard launch scroller column around the untouched .sr-wrap fragment;
     the serif page heading is the existing .sr-label, restyled in the
     launch-search CSS section rather than duplicated here. *)
  let content =
    Printf.sprintf "<div class='scroll'><div class='container container--list'>%s</div></div>" body
  in
  Components.launch_app_page ?user ~request ~rail_communities
    ~page_class:"launch-search" ~title ~content ()

(* === LEGAL / PRIVACY === *)

(* Single-section structure: plain-English human summary up top, then technical spec.
   Grounded in actual schema/auth.ml — no invented infrastructure or fictional DPO. *)
let privacy_page ?user request =
  let content = "
    <div class='max-w-2xl mx-auto mt-10 mb-16 px-4'>

      <h1 class='text-3xl font-extrabold text-gray-900 mb-2'>Privacy Policy</h1>
      <p class='text-sm text-gray-400 mb-10'>This page explains, in plain terms, what data Earde handles and why.</p>

      <div class='space-y-8 text-gray-700 leading-relaxed'>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>What Earde is</h2>
          <p>Earde is a community platform for technical communities. It combines live chat with durable discussion threads and a searchable archive.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>Data we may collect or store</h2>
          <p class='mb-3'>To operate the service, Earde may store:</p>
          <ul class='list-disc list-inside space-y-2 text-sm'>
            <li>Account information, such as your username and email address.</li>
            <li>Profile information you choose to add.</li>
            <li>Community content, posts, and comments you create.</li>
            <li>Chat messages you send.</li>
            <li>Session and authentication data needed to keep you signed in.</li>
            <li>Moderation records related to reports and enforcement actions.</li>
            <li>Operational and security logs.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>How we use data</h2>
          <ul class='list-disc list-inside space-y-2 text-sm'>
            <li>To operate and provide the service.</li>
            <li>To authenticate users and keep accounts secure.</li>
            <li>To display community content.</li>
            <li>To moderate abuse and enforce community rules.</li>
            <li>To maintain the security of the service.</li>
            <li>To debug problems and improve reliability.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>Cookies and sessions</h2>
          <p>Earde may use cookies or similar browser storage for login and session functionality and for basic operation of the site.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>Analytics and tracking</h2>
          <p>If analytics or tracking tools are added in the future, they should be disclosed here and configured deliberately.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>Your controls</h2>
          <p class='mb-3'>You can contact the operator of this site with any questions about your account or your data.</p>
          <p>From your <a href='/settings' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>account settings</a> you can update your profile or delete your account, and you can <a href='/export-data' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>export your data</a>.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>Changes to this page</h2>
          <p>This page may be updated as Earde changes.</p>
        </section>

      </div>
    </div>"
  in
  Components.layout ?user ~request ~title:"Privacy Policy" content

(* === MESSAGE PAGE === *)

(* Single shell for errors, successes, and info — avoids per-handler inline HTML
   fragments that diverge in style and don't inherit the shared layout/nav. *)
let msg_page ?user ?(auth=false) ~title ~message ~alert_type ~return_url request =
  (* auth:true renders the focused auth panel (auth.css) so confirm-email / verify /
     reset / login result pages match the new auth layout. The default (false) keeps
     the warm `Site card byte-for-byte for every non-auth caller. *)
  if auth then begin
    let icon_html = match alert_type with
      | "success" ->
          "<div class='auth-msg-icon auth-msg-icon--success'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M5 13l4 4L19 7'/></svg></div>"
      | "info" ->
          "<div class='auth-msg-icon auth-msg-icon--info'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><path d='M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z'/></svg></div>"
      | _ ->
          "<div class='auth-msg-icon auth-msg-icon--error'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M6 18L18 6M6 6l12 12'/></svg></div>"
    in
    let card = Printf.sprintf "
        <div class='auth-msg'>
          %s
          <h1 class='auth-title'>%s</h1>
          <p class='auth-msg-text'>%s</p>
          <a href='%s' class='auth-btn auth-btn--inline'>Go back</a>
        </div>"
      icon_html (Components.html_escape title) (Components.html_escape message) return_url
    in
    Components.auth_page ?user ~request ~title ~card ()
  end else
  (* Default (non-auth) callers: render the same focused cool-grey message panel as
     the auth branch (auth.css), via auth_page. This drops the warm `Site card +
     navbar + footer that used to interrupt the cool-grey app on every error/empty
     state. auth_page's brand mark links to /feed (recovery) and the panel keeps an
     explicit return-to-context link from return_url. Works logged-in or logged-out;
     no new CSS, no JS. *)
  let icon_html = match alert_type with
    | "success" ->
        "<div class='auth-msg-icon auth-msg-icon--success'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M5 13l4 4L19 7'/></svg></div>"
    | "info" ->
        "<div class='auth-msg-icon auth-msg-icon--info'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'><path d='M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z'/></svg></div>"
    | _ ->
        "<div class='auth-msg-icon auth-msg-icon--error'><svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2.5' stroke-linecap='round' stroke-linejoin='round'><path d='M6 18L18 6M6 6l12 12'/></svg></div>"
  in
  let card = Printf.sprintf "
        <div class='auth-msg'>
          %s
          <h1 class='auth-title'>%s</h1>
          <p class='auth-msg-text'>%s</p>
          <a href='%s' class='auth-btn auth-btn--inline'>Go back</a>
        </div>"
    icon_html (Components.html_escape title) (Components.html_escape message) return_url
  in
  Components.auth_page ?user ~request ~title ~card ()

(* === ADMIN === *)

(* Display-only heuristic for the admin dashboard: flags bot-like usernames (the
   signup incident produced runs of random handles). Deliberately conservative and
   imperfect — it only paints a UI chip, never gates an action or touches the DB. Pure,
   so it is unit-tested in test_earde.ml. A name is "random-looking" when digits dominate,
   it has no vowels at all, it has a long consonant run, or its vowel ratio is very low. *)
let looks_random_username raw =
  let s = String.lowercase_ascii raw in
  let n = String.length s in
  if n < 5 then false
  else begin
    let is_vowel c = c = 'a' || c = 'e' || c = 'i' || c = 'o' || c = 'u' in
    let letters = ref 0 and vowels = ref 0 and digits = ref 0 in
    let cur_run = ref 0 and max_run = ref 0 in
    String.iter (fun c ->
      if c >= '0' && c <= '9' then (incr digits; cur_run := 0)
      else if c >= 'a' && c <= 'z' then begin
        incr letters;
        if is_vowel c then (incr vowels; cur_run := 0)
        else (incr cur_run; if !cur_run > !max_run then max_run := !cur_run)
      end else cur_run := 0
    ) s;
    let digit_ratio = float_of_int !digits /. float_of_int n in
    let vowel_ratio = if !letters = 0 then 0.0 else float_of_int !vowels /. float_of_int !letters in
    digit_ratio >= 0.4
    || (!letters >= 5 && !vowels = 0)
    || !max_run >= 5
    || (!letters >= 6 && vowel_ratio < 0.15)
  end

(* Restyled admin dashboard: the focused cool-grey admin layout (Components.admin_page),
   NOT the legacy Site shell. Read-only operational panels (status / recent users /
   pending signups) plus the preserved global ban/unban action. Authorization is enforced
   by admin_dashboard_handler (is_admin session field); this renderer assumes it. All
   config status is a safe boolean/label — no secret value (site key, API key, URL) is
   ever rendered. *)
let admin_dashboard_page ?user ~signups_enabled
    ~(turnstile : [ `Configured | `Disabled | `Misconfigured ]) ~brevo_configured
    ~(recent_users : Db.admin_recent_user list) ~(pending : Db.pending_signup_row list)
    ~(banned_users : user list) request =
  let csrf_token = Dream.csrf_tag request in
  let esc = Components.html_escape in
  (* "recently created" cutoff as an ISO-ish string; created_at::text sorts lexically the
     same as chronologically, so a string compare avoids any timestamp parsing. *)
  let recent_cutoff =
    let tm = Unix.gmtime (Unix.gettimeofday () -. 86400.) in
    Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  let stat label cls value =
    Printf.sprintf
      "<div class='admin-stat'><span class='admin-stat-label'>%s</span><span class='admin-stat-val %s'>%s</span></div>"
      label cls value
  in
  let status_panel =
    let signups_row =
      if signups_enabled then stat "signups" "admin-stat-val--ok" "enabled"
      else stat "signups" "admin-stat-val--off" "closed"
    in
    let turnstile_row = match turnstile with
      | `Configured   -> stat "turnstile (bot check)" "admin-stat-val--ok"   "required &amp; configured"
      | `Disabled     -> stat "turnstile (bot check)" "admin-stat-val--off"  "disabled · dev bypass"
      | `Misconfigured -> stat "turnstile (bot check)" "admin-stat-val--bad" "misconfigured"
    in
    let brevo_row =
      if brevo_configured then stat "email (brevo)" "admin-stat-val--ok" "configured"
      else stat "email (brevo)" "admin-stat-val--warn" "not configured"
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Signup &amp; security status</h2>\
         <p class='admin-panel-desc'>Operational config flags only — no secret values are shown.</p>\
         <div class='admin-stats'>%s%s%s</div>\
       </section>"
      signups_row turnstile_row brevo_row
  in
  let recent_users_panel =
    let rows =
      if recent_users = [] then
        "<tr><td colspan='6' class='admin-empty'>No users yet.</td></tr>"
      else
        String.concat "\n" (List.map (fun (u : Db.admin_recent_user) ->
          let badges =
            (if u.is_admin  then "<span class='admin-badge admin-badge--admin'>admin</span>" else "")
            ^ (if u.is_banned then "<span class='admin-badge admin-badge--banned'>banned</span>" else "")
          in
          let total = u.post_count + u.comment_count + u.message_count in
          let flags =
            let fs =
              (if total = 0 then ["<span class='admin-flag'>no activity</span>"] else [])
              @ (if looks_random_username u.username then ["<span class='admin-flag'>random name?</span>"] else [])
              @ (if u.created_at >= recent_cutoff then ["<span class='admin-flag admin-flag--quiet'>new &lt;24h</span>"] else [])
            in
            if fs = [] then "<span class='admin-cell-muted'>—</span>" else String.concat "" fs
          in
          let num c = Printf.sprintf "<td class='admin-num%s'>%d</td>" (if c = 0 then " admin-num--zero" else "") c in
          Printf.sprintf
            "<tr>\
               <td><a class='admin-user-link' href='/u/%s'>%s</a>%s</td>\
               <td class='admin-cell-muted'>%s</td>%s%s%s\
               <td>%s</td>\
             </tr>"
            (esc u.username) (esc u.username) badges
            (Components.time_ago u.created_at)
            (num u.post_count) (num u.comment_count) (num u.message_count)
            flags
        ) recent_users)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Recent users</h2>\
         <p class='admin-panel-desc'>Latest %d accounts by signup time, with activity counts and quick suspicious-signal flags.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>User</th><th>Joined</th><th>Posts</th><th>Comments</th><th>Msgs</th><th>Signals</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      (List.length recent_users) rows
  in
  let pending_panel =
    let rows =
      if pending = [] then
        "<tr><td colspan='5' class='admin-empty'>No active pending signups.</td></tr>"
      else
        String.concat "\n" (List.map (fun (p : Db.pending_signup_row) ->
          let ip = match p.ip_address with Some s when s <> "" -> esc s | _ -> "—" in
          Printf.sprintf
            "<tr>\
               <td class='admin-cell-mono'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-muted'>%s</td>\
               <td class='admin-cell-mono'>%s</td>\
             </tr>"
            (esc p.username) (esc p.email)
            (Components.time_ago p.created_at) (Components.time_ago p.expires_at) ip
        ) pending)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Pending signups</h2>\
         <p class='admin-panel-desc'>Unconfirmed signups still within their 24h window (latest %d). These have not become user accounts yet.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>Username</th><th>Email</th><th>Requested</th><th>Expires</th><th>IP</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      (List.length pending) rows
  in
  let banned_panel =
    let rows =
      if banned_users = [] then
        "<tr><td colspan='3' class='admin-empty'>No users are currently globally banned.</td></tr>"
      else
        String.concat "\n" (List.map (fun (u : user) ->
          Printf.sprintf
            "<tr>\
               <td><a class='admin-user-link' href='/u/%s'>%s</a></td>\
               <td class='admin-cell-muted'>%s</td>\
               <td>\
                 <form class='admin-act-form' action='/admin/unban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">\
                   %s\
                   <button type='submit' class='admin-btn-unban'>Unban</button>\
                 </form>\
               </td>\
             </tr>"
            (esc u.username) (esc u.username) (esc u.email) u.id (esc u.username) csrf_token
        ) banned_users)
    in
    Printf.sprintf
      "<section class='admin-panel'>\
         <h2 class='admin-panel-title'>Globally banned users</h2>\
         <p class='admin-panel-desc'>These accounts are blocked from logging in and posting anywhere on Earde.</p>\
         <div class='admin-table-wrap'><table class='admin-table ph-no-capture'>\
           <thead><tr><th>User</th><th>Email</th><th>Action</th></tr></thead>\
           <tbody>%s</tbody>\
         </table></div>\
       </section>"
      rows
  in
  let body = Printf.sprintf
    "<div class='admin-wrap'>\
       <div class='admin-head'>\
         <h1 class='admin-h1'>admin <span class='accent'>·</span> dashboard</h1>\
         <a class='admin-head-link' href='/earde-hq-dashboard'>KPI dashboard &rarr;</a>\
       </div>\
       %s%s%s%s\
     </div>"
    status_panel recent_users_panel pending_panel banned_panel
  in
  Components.admin_page ?user ~request ~noindex:true ~title:"Admin Dashboard" ~body ()

(* === MODERATION LOG === *)

let mod_log_page ?user ?(noindex=false) ?(rail_communities = [])
    ~(can_access_settings : bool) ~(channels : channel list)
    ~(sections : community_section list)
    ~(community : Db.community) (actions : Db.mod_action list) request =
  let esc = Components.html_escape in
  (* Mod log is member-visible, not mod-only. Send viewers who can reach settings back into the
     moderation panel; send everyone else back to the community home. *)
  let back_link =
    if can_access_settings then
      Printf.sprintf "<a href='/c/%s/settings?panel=moderation' class='cm-back'>&larr; Back to settings</a>" (esc community.slug)
    else
      Printf.sprintf "<a href='/c/%s' class='cm-back'>&larr; Back to community</a>" (esc community.slug)
  in
  let render_action (a : Db.mod_action) =
    let target_html = match a.target_id with
      | None -> ""
      | Some tid -> Printf.sprintf "<span class='cm-table-target'> &middot; target #%d</span>" tid
    in
    (* Admin overrides are logged with an "admin_"-prefixed action type; flag them
       distinctly so a global-admin action reads differently from a community-mod one. *)
    let tag_cls =
      if String.length a.action_type >= 6 && String.sub a.action_type 0 6 = "admin_"
      then "cm-action-tag cm-action-tag--admin" else "cm-action-tag"
    in
    Printf.sprintf "
    <tr>
        <td class='cm-table-when'>%s</td>
        <td><a href='/u/%s' class='cm-table-actor'>%s</a></td>
        <td><span class='%s'>%s</span>%s</td>
        <td class='cm-table-reason'>%s</td>
    </tr>"
      (Components.time_ago a.created_at)
      (esc a.moderator_username) (esc a.moderator_username)
      tag_cls (esc a.action_type) target_html
      (esc a.reason)
  in
  let table_body =
    if actions = [] then
      "<tr><td colspan='4' class='cm-table-empty'>No moderation actions recorded yet.</td></tr>"
    else String.concat "\n" (List.map render_action actions)
  in
  let content = Printf.sprintf "
    <div class='cm-wrap'>
        <div class='cm-head'>
            <h1 class='cm-h1'>Moderation <span class='accent'>log</span></h1>
            <span class='mono launch-modlog-ctx'>/c/%s</span>
            %s
        </div>
        <section class='cm-panel'>
            <h2 class='cm-panel-title'>Action history</h2>
            <p class='cm-panel-desc'>Public record of moderator actions in this community.</p>
            <div class='cm-table-wrap'>
                <table class='cm-table'>
                    <thead>
                        <tr>
                            <th>When</th>
                            <th>Moderator</th>
                            <th>Action</th>
                            <th>Reason</th>
                        </tr>
                    </thead>
                    <tbody>%s</tbody>
                </table>
            </div>
        </section>
    </div>"
    (esc community.slug)
    back_link
    table_body
  in
  (* Cartographic launch shell (pass 14A): the same four-pane chrome as the
     sibling community routes, wrapping the moderation ledger verbatim — the
     cm-* event rows, empty row and .cm-table-reason replay-mask class are
     treated as pinned (analytics.js masks .cm-table-reason by name), so only
     this outer document and the additive mono /c/:slug context above changed.
     The sidebar reuses the shared knowledge grammar with Moderation log
     active; can_manage mirrors the handler's settings gate (admin ||
     moderator), which here only decorates navigation — the route itself
     stays public exactly as modlog_handler allows. [rail_communities]
     carries the viewer's joined communities exactly as the handler loaded
     them (post-authorization only); ordering, dedup by slug, and the single
     active marker stay owned by the shared launch doc builder. Replay
     privacy keeps the existing inner ph-no-capture guard for private
     communities (the launch shell additionally marks .shell, exactly like
     the sibling routes). *)
  let sidebar =
    launch_knowledge_sidebar ~community ~channels ~sections
      ~moderation_log_active:true ~can_manage:can_access_settings ()
  in
  Components.launch_community_page ?user ~noindex ~request ~rail_communities
    ~community ~sidebar
    ~page_class:"launch-community-modlog"
    ~title:(community.name ^ " — Mod Log")
    ~content:(Components.private_replay_guard ~community content) ()

(* Standalone HTML — intentionally outside Components.layout to prevent nav/JS
   assets from loading on an admin-only internal page that needs no public shell. *)
let hq_dashboard_page ((views, unique_visitors, signups), (content, _active)) ~dau_mau_ratio ~start_date ~end_date =
  Printf.sprintf {html|<!DOCTYPE html>
<html lang='en'>
<head>
    <meta charset='UTF-8'>
    <meta name='viewport' content='width=device-width, initial-scale=1.0'>
    <title>Earde HQ - Mission Control</title>
    <script src='https://cdn.tailwindcss.com'></script>
</head>
<body class='bg-gray-950 text-green-400 font-mono min-h-screen p-8'>
    <div class='max-w-7xl mx-auto'>

        <header class='flex justify-between items-end border-b border-green-900 pb-4 mb-6'>
            <div>
                <h1 class='text-4xl font-bold tracking-tighter text-white'>Earde <span class='text-green-500'>SYS.CORE</span></h1>
                <p class='text-green-700 text-sm mt-1'>range: %s &rarr; %s</p>
            </div>
            <div class='text-right'>
                <div class='text-3xl font-bold text-white'>%.1f%%</div>
                <div class='text-xs text-green-700'>RETENTION</div>
            </div>
        </header>

        <form method='GET' action='/earde-hq-dashboard' class='mb-8 bg-gray-900 border border-green-900 rounded-xl p-4'>
            <div class='flex flex-wrap gap-2 items-end'>
                <div class='flex gap-2 flex-wrap'>
                    <button type='button' onclick='setRange(0,0)'
                        class='px-3 py-1.5 text-xs border border-green-800 text-green-500 rounded hover:bg-green-900 hover:text-white transition-colors'>
                        Today
                    </button>
                    <button type='button' onclick='setRange(6,0)'
                        class='px-3 py-1.5 text-xs border border-green-800 text-green-500 rounded hover:bg-green-900 hover:text-white transition-colors'>
                        Last 7 Days
                    </button>
                    <button type='button' onclick='setRange(29,0)'
                        class='px-3 py-1.5 text-xs border border-green-800 text-green-500 rounded hover:bg-green-900 hover:text-white transition-colors'>
                        Last 30 Days
                    </button>
                    <button type='button' onclick='setAllTime()'
                        class='px-3 py-1.5 text-xs border border-green-800 text-green-500 rounded hover:bg-green-900 hover:text-white transition-colors'>
                        All Time
                    </button>
                </div>
                <div class='flex gap-2 items-center ml-auto'>
                    <label class='text-xs text-green-700'>FROM</label>
                    <input type='date' name='start' value='%s'
                        class='bg-gray-800 border border-green-900 text-green-300 text-xs rounded px-2 py-1.5 focus:outline-none focus:border-green-500'>
                    <label class='text-xs text-green-700'>TO</label>
                    <input type='date' name='end' value='%s'
                        class='bg-gray-800 border border-green-900 text-green-300 text-xs rounded px-2 py-1.5 focus:outline-none focus:border-green-500'>
                    <button type='submit'
                        class='px-4 py-1.5 text-xs bg-green-900 text-green-300 border border-green-700 rounded hover:bg-green-800 hover:text-white transition-colors'>
                        Apply
                    </button>
                </div>
            </div>
        </form>

        <div class='grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-6'>

            <div class='bg-gray-900 border border-green-900 p-6 rounded-xl shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>PAGE VIEWS</h2>
                <div class='text-5xl font-bold text-white mb-2'>%d</div>
                <div class='text-xs text-green-700 mb-3'>TOTAL IN RANGE</div>
                <div class='border-t border-green-900 pt-3'>
                    <div class='text-lg font-bold text-green-400'>%d</div>
                    <div class='text-xs text-green-700'>UNIQUE VISITORS</div>
                </div>
                <p class='text-xs text-gray-500 mt-3 leading-relaxed'>Total number of pages loaded. Measures raw traffic volume and top-of-funnel reach.</p>
            </div>

            <div class='bg-gray-900 border border-green-900 p-6 rounded-xl shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>NEW SIGNUPS</h2>
                <div class='text-5xl font-bold text-white mb-2'>%d</div>
                <div class='text-xs text-green-700'>TOTAL IN RANGE</div>
                <p class='text-xs text-gray-500 mt-3 leading-relaxed'>Total registered users. Measures our ability to convert casual visitors into community members.</p>
            </div>

            <div class='bg-gray-900 border border-green-900 p-6 rounded-xl shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>CONTENT ACTIVITY</h2>
                <div class='text-5xl font-bold text-white mb-2'>%d</div>
                <div class='text-xs text-green-700'>POSTS + COMMENTS IN RANGE</div>
                <p class='text-xs text-gray-500 mt-3 leading-relaxed'>Total posts and comments created. Indicates if the platform is actively generating discussion or if it&apos;s read-only.</p>
            </div>

            <div class='bg-gray-900 border border-green-900 p-6 rounded-xl shadow-[0_0_15px_rgba(34,197,94,0.1)] relative overflow-hidden'>
                <div class='absolute top-0 right-0 w-16 h-16 bg-green-500 opacity-10 rounded-bl-full'></div>
                <h2 class='text-green-500 text-sm font-bold mb-4 tracking-widest'>RETENTION</h2>
                <div class='text-5xl font-bold text-white mb-2'>%.1f%%</div>
                <div class='text-xs text-green-700'>DAU/MAU RATIO</div>
                <p class='text-xs text-gray-500 mt-3 leading-relaxed'>Percentage of monthly users who return daily. A DAU/MAU ratio &gt; 20&percnt; indicates strong user retention.</p>
            </div>

        </div>
    </div>

    <script>
        function isoDate(d) {
            return d.toISOString().slice(0, 10);
        }
        function setRange(daysBack, daysEnd) {
            var now = new Date();
            var end = new Date(now);
            end.setDate(end.getDate() - daysEnd);
            var start = new Date(end);
            start.setDate(start.getDate() - daysBack);
            document.querySelector('input[name=start]').value = isoDate(start);
            document.querySelector('input[name=end]').value = isoDate(end);
            document.querySelector('form').submit();
        }
        function setAllTime() {
            document.querySelector('input[name=start]').value = '1970-01-01';
            document.querySelector('input[name=end]').value = '2099-12-31';
            document.querySelector('form').submit();
        }
    </script>
</body>
</html>|html}
  start_date end_date (dau_mau_ratio *. 100.0)
  start_date end_date
  views unique_visitors
  signups
  content
  (dau_mau_ratio *. 100.0)
