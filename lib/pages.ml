open Db

(* === CORE FEED === *)

let index ?user user_votes current_page sort_mode ~feed_type ~admin_usernames ~moderated_communities (posts : post list) (user_communities : community list) request =
  let has_next = List.length posts = 20 in
  (* base_url drives pagination and sort links so they stay within the correct feed *)
  let base_url = if feed_type = "home" then "/" else "/all" in

  let posts_html =
    if posts = [] then
      "<div class='text-center py-10 text-gray-500 border border-dashed border-[#E0D9CC] rounded-xl'>It's quiet here. Too quiet. <br><a href='/new-community' class='text-[#C94C4C] underline'>Create a community</a> and start posting!</div>"
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
        <div class='w-full lg:w-1/4'><div class='bg-white p-5 rounded-xl border border-[#E0D9CC] sticky top-20'><h2 class='text-sm font-semibold text-gray-800 mb-1'>Earde</h2><p class='text-xs text-gray-500 mb-4'>Your personal frontpage.</p><div class='flex flex-col space-y-2'><a href='/new-post' class='w-full bg-[#C94C4C] text-white text-center py-2 rounded-xl font-semibold text-sm hover:bg-[#A83A3A] transition'>Create Post</a><a href='/new-community' class='w-full bg-white text-[#C94C4C] border border-[#C94C4C] text-center py-2 rounded-xl font-semibold text-sm hover:bg-[#F0EDE4] transition'>Create Community</a></div></div><div class='mt-6 bg-blue-50 border border-blue-100 rounded-xl p-4 shadow-sm'><h3 class='text-sm font-bold text-blue-900 mb-1'>Talk to me!</h3><p class='text-xs text-blue-800 mb-3 leading-relaxed'>For feature requests, ideas, critiques, if you are a Reddit mod and want to become a mod on the specular community here, or just to say hi!</p><a href='https://t.me/tolwiz' target='_blank' rel='noopener noreferrer' class='w-full flex items-center justify-center gap-2 bg-blue-600 hover:bg-blue-700 text-white text-sm font-semibold py-2 rounded-xl transition-colors'>&#128172; Text me (the dev)!</a></div></div>
    </div>"
    sidebar_html feed_title feed_tabs sort_menu posts_html prev_btn current_page next_btn
  in
  Components.layout ?user ~request ~title:feed_title content

(* === AUTHENTICATION === *)

let signup_form ?user ?error request =
  let csrf_token = Dream.csrf_tag request in
  let error_html = match error with
    | None -> ""
    | Some msg -> Printf.sprintf "<p class='text-sm text-red-600 bg-red-50 border border-red-200 rounded-xl p-3'>%s</p>" msg
  in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-xl shadow-md mt-10'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800 text-center'>Join Earde</h1>

        <form action='/signup' method='POST' class='space-y-4'>
            %s
            %s

            <div>
                <label class='block text-sm font-medium text-gray-700'>Username</label>
                <input type='text' name='username' required
                       class='mt-1 block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='Choose a unique username'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700'>Email Address</label>
                <input type='email' name='email' required
                       class='mt-1 block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='you@example.com'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700'>Password</label>
                <input type='password' name='password' required
                       class='mt-1 block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='Minimum 8 characters'>
            </div>

            <div class='flex items-start mt-4 mb-6'>
                <div class='flex items-center h-5'>
                    <input id='privacy' name='privacy' type='checkbox' required
                           class='w-4 h-4 border border-[#D0C9BC] rounded bg-gray-50 focus:ring-2 focus:ring-[#C94C4C] cursor-pointer'>
                </div>
                <label for='privacy' class='ml-2 text-sm font-medium text-gray-700 cursor-pointer'>
                    I agree to the <a href='/privacy' target='_blank' class='text-[#C94C4C] hover:underline font-bold'>Privacy Policy</a> and consent to data processing.
                </label>
            </div>

            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded-xl hover:bg-[#A83A3A] font-semibold transition'>
                Sign Up
            </button>
        </form>

        <div class='mt-6 text-center text-sm'>
            <p class='text-gray-600'>Already have an account?</p>
            <a href='/login' class='font-medium text-[#C94C4C] hover:text-[#C94C4C]'>Log in</a>
        </div>
    </div>"
    csrf_token error_html
  in
  Components.layout ?user ~request ~title:"Sign Up" content

let login_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] mt-10'>
        <h1 class='text-2xl font-bold mb-6 text-gray-900 text-center'>Welcome back</h1>
        <form action='/login' method='POST' class='space-y-4'>
            %s
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Username or Email</label>
                <input type='text' name='identifier' required class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border' placeholder='tolwiz or tolwiz@example.com'>
            </div>
            <div>
                <div class='flex justify-between items-center mb-1'>
                    <label class='block text-sm font-medium text-gray-700'>Password</label>
                    <a href='/forgot-password' class='text-xs text-[#C94C4C] hover:text-indigo-800 hover:underline tabindex='-1'>Forgot password?</a>
                </div>
                <input type='password' name='password' required class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
            </div>
            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded font-bold hover:bg-[#A83A3A] transition shadow-sm'>Log In</button>
        </form>
        <div class='mt-6 text-center text-sm text-gray-600'>
            Don't have an account? <a href='/signup' class='text-[#C94C4C] font-bold hover:underline'>Sign up</a>
        </div>
    </div>" csrf_token
  in
  Components.layout ~title:"Log In" ?user ~request content

let forgot_password_page request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] mt-10'>
        <h1 class='text-2xl font-bold mb-2 text-gray-900 text-center'>Forgot Password?</h1>
        <p class='text-gray-600 text-sm text-center mb-6'>Enter your email address and we'll send you a reset link.</p>
        <form action='/forgot-password' method='POST' class='space-y-4'>
            %s
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Email address</label>
                <input type='email' name='email' required class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border' placeholder='you@example.com'>
            </div>
            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded font-bold hover:bg-[#A83A3A] transition shadow-sm'>Send Reset Link</button>
        </form>
        <div class='mt-4 text-center text-sm text-gray-500'>
            <a href='/login' class='text-[#C94C4C] hover:underline'>Back to login</a>
        </div>
    </div>" csrf_token
  in Components.layout ~noindex:true ~request ~title:"Forgot Password" content

let reset_password_page ~token ?error request =
  let csrf_token = Dream.csrf_tag request in
  let error_html = match error with
    | None -> ""
    | Some msg -> Printf.sprintf "<div class='bg-red-50 border border-red-200 text-red-700 rounded p-3 text-sm mb-4'>%s</div>" msg
  in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] mt-10'>
        <h1 class='text-2xl font-bold mb-2 text-gray-900 text-center'>Set New Password</h1>
        <p class='text-gray-600 text-sm text-center mb-6'>Enter a new password for your account.</p>
        %s
        <form action='/reset-password' method='POST' class='space-y-4'>
            %s
            <input type='hidden' name='token' value='%s'>
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>New password</label>
                <input type='password' name='password' required minlength='8' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
            </div>
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Confirm new password</label>
                <input type='password' name='confirm_password' required minlength='8' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
            </div>
            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded font-bold hover:bg-[#A83A3A] transition shadow-sm'>Reset Password</button>
        </form>
    </div>" error_html csrf_token token
  in Components.layout ~noindex:true ~request ~title:"Reset Password" content

(* === COMMUNITY === *)

let new_community_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-xl shadow-md border border-[#E0D9CC]'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800'>Create a New Community</h1>

        <form action='/communities' method='POST' class='space-y-6'>
            %s

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Community Name</label>
                <input type='text' name='name' required
                       class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='e.g., Italian Cuisine'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>URL Slug</label>
                <div class='flex rounded-xl shadow-sm'>
                    <span class='inline-flex items-center px-3 rounded-l-md border border-r-0 border-[#D0C9BC] bg-gray-50 text-gray-500 text-sm'>
                        /c/
                    </span>
                    <input type='text' name='slug' required
                           class='flex-1 block w-full rounded-none rounded-r-md border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-[#C94C4C] p-2 border'
                           placeholder='italian-cuisine'>
                </div>
                <p class='mt-1 text-xs text-gray-500'>Lowercase, no spaces.</p>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Description</label>
                <textarea name='description' rows='3'
                          class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'></textarea>
            </div>

            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded-xl hover:bg-[#A83A3A] font-semibold transition'>
                Create Community
            </button>
        </form>
    </div>"
  csrf_token
  in
  Components.layout ?user ~request ~title:"New Community" content

let community_page ?user ~is_member ~is_current_user_mod ~is_current_user_top_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities user_votes current_page sort_mode (community : community) (posts : post list) request =
  let csrf_token = Dream.csrf_tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let has_next = List.length posts = 20 in

  let posts_html =
    if posts = [] then "<div class='bg-gray-50 p-12 text-center rounded-xl border border-dashed border-[#D0C9BC] text-gray-500'>No posts yet. Be the first to share something!</div>"
    else String.concat "\n" (List.map (Components.render_post ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames request user_votes) posts)
  in

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='/c/%s?sort=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-[#EDE9DF] transition'>&larr; Previous</a>" community.slug sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='/c/%s?sort=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-[#EDE9DF] transition'>Next &rarr;</a>" community.slug sort_mode (current_page + 1) in

  let get_sort_class s = if s = sort_mode then "text-[#C94C4C] border-b-2 border-[#C94C4C] pb-1" else "text-gray-500 hover:text-gray-800 transition" in
  let sort_menu = Printf.sprintf "
    <div class='flex space-x-6 mb-6 px-2 border-b border-[#E0D9CC] mt-6'>
        <a href='/c/%s?sort=hot' class='font-bold text-sm tracking-wide uppercase %s'>🔥 Hot</a>
        <a href='/c/%s?sort=new' class='font-bold text-sm tracking-wide uppercase %s'>✨ New</a>
        <a href='/c/%s?sort=top' class='font-bold text-sm tracking-wide uppercase %s'>🏆 Top</a>
    </div>" community.slug (get_sort_class "hot") community.slug (get_sort_class "new") community.slug (get_sort_class "top")
  in

  let membership_btn =
    match user with
    | None -> ""
    | Some _ ->
        if is_member then Printf.sprintf "<form action='/leave' method='POST' class='m-0 p-0'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='rounded-full px-5 py-1.5 border border-[#D0C9BC] hover:bg-gray-50 text-sm font-medium text-gray-700 transition'>Leave</button></form>" csrf_token community.id community.slug
        else Printf.sprintf "<form action='/join' method='POST' class='m-0 p-0'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='rounded-full px-5 py-1.5 bg-[#C94C4C] text-white text-sm font-semibold hover:bg-[#A83A3A] transition'>Join</button></form>" csrf_token community.id community.slug
  in

  (* Admins see the edit button without needing mod status — global authority. *)
  let settings_btn =
    if is_current_user_mod || is_admin then
      Printf.sprintf "<a href='/c/%s/settings' class='rounded-full px-4 py-1.5 border border-[#D0C9BC] hover:bg-gray-50 text-sm font-medium text-gray-700 transition'>Edit Community</a>" community.slug
    else ""
  in

  (* Create Post shortcut lives in the header, not the sidebar, for immediate access. *)
  let create_post_btn =
    match user with
    | None -> ""
    | Some _ ->
        Printf.sprintf "<a href='/new-post?community=%s' class='rounded-full px-5 py-1.5 bg-[#C94C4C] text-white text-sm font-semibold hover:bg-[#A83A3A] transition'>+ Post</a>" community.slug
  in

  let banner_html =
    match community.banner_url with
    | Some url when url <> "" ->
        Printf.sprintf "<div class='h-24 md:h-40 w-full bg-gray-100 rounded-2xl overflow-hidden'><img src='%s' class='w-full h-full object-cover' alt='banner'></div>" url
    | _ ->
        "<div class='h-24 md:h-40 w-full bg-gradient-to-r from-[#EDE9DF] to-[#E8E2D9] rounded-2xl'></div>"
  in

  let avatar_html =
    match community.avatar_url with
    | Some url when url <> "" ->
        Printf.sprintf "<img src='%s' class='w-20 h-20 md:w-24 md:h-24 rounded-full border-4 border-white bg-white shadow-sm object-cover'>" url
    | _ ->
        Printf.sprintf "<div class='w-20 h-20 md:w-24 md:h-24 rounded-full border-4 border-white bg-[#69C3D2] flex items-center justify-center text-white text-2xl font-bold shadow-sm'>%s</div>"
          (String.uppercase_ascii (String.sub community.name 0 1))
  in

  let mods_sidebar_html =
    if mod_usernames = [] then "<p class='text-xs text-gray-400 italic'>No moderators yet.</p>"
    else
      let links = String.concat "\n" (List.map (fun u ->
        Printf.sprintf "<li><a href='/u/%s' class='text-sm text-gray-700 hover:text-[#C94C4C] transition'>u/%s</a></li>" (Components.html_escape u) (Components.html_escape u)
      ) mod_usernames) in
      Printf.sprintf "<ul class='space-y-1'>%s</ul>" links
  in

  (* Three separate sidebar cards: clearer hierarchy than one combined card. *)
  let community_info_card = Printf.sprintf "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h2 class='font-bold text-gray-900 mb-1'>%s</h2>
        <div class='text-xs text-[#C94C4C] font-mono mb-3'>/c/%s</div>
        <p class='text-sm text-gray-600'>%s</p>
        <a href='/c/%s/modlog' class='mt-4 flex items-center gap-2 text-sm text-gray-400 hover:text-gray-600 transition-colors'>
            <span>&#128220;</span><span>Public Modlog</span>
        </a>
    </div>"
    (Components.html_escape community.name) (Components.html_escape community.slug)
    (Components.html_escape (Option.value ~default:"No description." community.description))
    (Components.html_escape community.slug)
  in

  let rules_card =
    match community.rules with
    | Some rules when rules <> "" ->
        Printf.sprintf "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-3'>Rules</h3>
        <p class='text-xs text-gray-600 whitespace-pre-wrap'>%s</p>
    </div>" (Components.html_escape rules)
    | _ -> ""
  in

  (* Manage Mods link: top_mod role or admin authority required — regular mods
     cannot appoint or demote peers, preventing collusion against the council. *)
  let manage_mods_link =
    if is_current_user_top_mod || is_admin then
      Printf.sprintf "<div class='mt-3'><a href='/c/%s/manage-mods' class='text-xs text-[#C94C4C] hover:underline font-semibold'>Manage Moderators &rarr;</a></div>" (Components.html_escape community.slug)
    else ""
  in

  (* Toggle downvotes: exposed only to top_mod/admin to prevent vote manipulation arms races
     by regular mods who have less community-wide accountability. *)
  let toggle_downvotes_card =
    if is_current_user_top_mod || is_admin then
      let (indicator, next_val, label) =
        if community.allow_downvotes then ("🔴", "false", "Disable Downvotes")
        else ("🟢", "true", "Enable Downvotes")
      in
      Printf.sprintf "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-3'>Mod Tools</h3>
        <form action='/c/%s/toggle_downvotes' method='POST' class='m-0 p-0'>
            %s
            <input type='hidden' name='allow_downvotes' value='%s'>
            <button type='submit' class='text-xs font-semibold text-gray-700 hover:text-gray-900 transition'>%s %s</button>
        </form>
    </div>"
      (Components.html_escape community.slug) csrf_token next_val indicator label
    else ""
  in

  let mods_card = Printf.sprintf "
    <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-5'>
        <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-3'>Moderators</h3>
        %s
        %s
    </div>"
    mods_sidebar_html manage_mods_link
  in

  let content = Printf.sprintf "
    <div class='flex gap-6'>
        <div class='w-60 hidden lg:block shrink-0 self-start sticky top-20'>
            %s
        </div>
        <div class='flex-1 flex flex-col min-w-0'>
            <div>
                %s
                <div class='flex items-end justify-between px-2 -mt-10 relative z-10 mb-4'>
                    <div class='flex items-end gap-4'>
                        %s
                        <div class='mb-2'>
                            <h1 class='text-2xl font-bold text-gray-900'>%s</h1>
                        </div>
                    </div>
                    <div class='flex items-center gap-2 mb-3'>
                        %s
                        %s
                        %s
                    </div>
                </div>
            </div>
            <div class='flex items-start gap-6'>
                <div class='flex-1 min-w-0'>
                    %s
                    <div>%s</div>
                    <div class='flex justify-between items-center mt-8 mb-4'>
                        <div>%s</div><div class='text-sm text-gray-500 font-bold'>Page %d</div><div>%s</div>
                    </div>
                </div>
                <div class='w-80 hidden lg:flex flex-col gap-6 self-start sticky top-24 h-[calc(100vh-6rem)] overflow-y-auto pb-8 [&::-webkit-scrollbar]:w-1.5 [&::-webkit-scrollbar-track]:bg-transparent [&::-webkit-scrollbar-thumb]:bg-transparent hover:[&::-webkit-scrollbar-thumb]:bg-gray-300 [&::-webkit-scrollbar-thumb]:rounded-full'>
                    %s
                    %s
                    %s
                    %s
                </div>
            </div>
        </div>
    </div>"
    (Components.left_sidebar ?user ~moderated_communities user_communities)
    banner_html
    avatar_html
    (Components.html_escape community.name)
    create_post_btn settings_btn membership_btn
    sort_menu posts_html
    prev_btn current_page next_btn
    community_info_card rules_card mods_card toggle_downvotes_card
  in
  Components.layout ?user ~request ~title:community.name content

let community_settings_page ?user ~(community : community) ~(mods : user list) ~(banned_users : user list) request =
  let csrf_token = Dream.csrf_tag request in
  (* mods param retained in signature for backward compat with community_settings_handler
     which still passes it; unused here since mod management moved to manage_mods_page. *)
  let _ = mods in

  let banned_section =
    if banned_users = [] then
      "<p class='text-gray-500 italic text-sm'>No users are currently banned from this community.</p>"
    else
      let rows = String.concat "\n" (List.map (fun (b : user) ->
        Printf.sprintf "
          <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
            <a href='/u/%s' class='font-medium text-gray-800 hover:text-[#C94C4C] hover:underline'>u/%s</a>
            <form action='/unban-community-user' method='POST' class='inline m-0 p-0'>
              %s
              <input type='hidden' name='target_user_id' value='%d'>
              <input type='hidden' name='community_id' value='%d'>
              <input type='hidden' name='community_slug' value='%s'>
              <button type='submit' class='text-xs text-green-600 hover:text-green-800 font-bold border border-green-200 px-2 py-1 rounded transition'>Unban</button>
            </form>
          </li>"
          (Components.html_escape b.username) (Components.html_escape b.username) csrf_token b.id community.id (Components.html_escape community.slug)
      ) banned_users) in
      Printf.sprintf "<ul>%s</ul>" rows
  in

  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto'>
      <div class='flex items-center justify-between mb-6'>
        <h1 class='text-2xl font-bold text-gray-900'>&#x2699;&#xFE0F; /c/%s Settings</h1>
        <a href='/c/%s' class='text-sm text-[#C94C4C] hover:underline'>&larr; Back to Community</a>
      </div>

      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Edit Community</h2>
        <form action='/update-community' method='POST' enctype='multipart/form-data' class='space-y-4'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='hidden' name='community_slug' value='%s'>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Description</label>
            <textarea name='description' rows='3'
                      class='w-full rounded-xl border border-[#D0C9BC] p-2 text-sm focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none'>%s</textarea>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Community Rules</label>
            <textarea name='rules' rows='5'
                      class='w-full rounded-xl border border-[#D0C9BC] p-2 text-sm focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none'>%s</textarea>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Avatar Image</label>
            <input type='hidden' name='existing_avatar_url' value='%s'>
            <input type='file' name='avatar_url' accept='image/*'
                   class='w-full rounded-xl border border-[#D0C9BC] p-2 text-sm focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none'>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Banner Image</label>
            <input type='hidden' name='existing_banner_url' value='%s'>
            <input type='file' name='banner_url' accept='image/*'
                   class='w-full rounded-xl border border-[#D0C9BC] p-2 text-sm focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none'>
          </div>
          <button type='submit' class='bg-[#C94C4C] text-white px-4 py-2 rounded-xl font-semibold text-sm hover:bg-[#A83A3A] transition'>
            Save Changes
          </button>
        </form>
      </div>

      <a href='/c/%s/manage-mods'
         class='flex items-center justify-between bg-white rounded-xl shadow-sm border border-[#C94C4C] p-5 mb-6 group hover:bg-[#F0EDE4] transition'>
        <div>
          <p class='text-base font-bold text-gray-900 group-hover:text-[#C94C4C] transition'>Manage Moderators Team</p>
          <p class='text-sm text-gray-500 mt-0.5'>Add, promote, and remove moderators &mdash; Council of Equals governance.</p>
        </div>
        <span class='text-[#C94C4C] text-xl font-bold'>&rarr;</span>
      </a>

      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Banned Users</h2>
        %s
        <form action='/ban-community-user' method='POST' class='flex items-center space-x-3 mt-4'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='text' name='target_username' required placeholder='Username to ban'
                 class='flex-1 rounded-xl border-[#D0C9BC] shadow-sm focus:border-[#C94C4C] p-2 border text-sm'>
          <button type='submit' class='bg-red-600 text-white px-4 py-2 rounded font-bold text-sm hover:bg-red-700 transition shadow-sm'>
            Ban User
          </button>
        </form>
      </div>
    </div>"
    community.slug community.slug
    csrf_token community.id community.slug
    (Components.html_escape (Option.value ~default:"" community.description))
    (Components.html_escape (Option.value ~default:"" community.rules))
    (Components.html_escape (Option.value ~default:"" community.avatar_url))
    (Components.html_escape (Option.value ~default:"" community.banner_url))
    community.slug
    banned_section csrf_token community.id
  in
  Components.layout ?user ~request ~title:(Printf.sprintf "Settings — /c/%s" community.slug) content

let manage_mods_page ?user ~is_admin ~current_user_role ~(community : community) ~(mods : moderator_entry list) request =
  let csrf_token = Dream.csrf_tag request in

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
          <form action='/c/%s/manage-mods/remove' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Demote this Top Mod? They will be removed from the council.')\">
            %s
            <input type='hidden' name='target_user_id' value='%d'>
            <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold border border-red-200 px-2 py-1 rounded transition'>Remove</button>
          </form>"
          (Components.html_escape community.slug) csrf_token m.user_id
      else "<span class='text-xs text-gray-400 italic'>Top Mod</span>"
    in
    Printf.sprintf "
      <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
        <div><a href='/u/%s' class='font-medium text-gray-800 hover:text-[#C94C4C] hover:underline'>u/%s</a>
        <span class='ml-2 text-xs bg-[#DFF5F8] text-[#69C3D2] px-1.5 py-0.5 rounded font-semibold'>Top Mod</span></div>
        %s
      </li>"
      (Components.html_escape m.username) (Components.html_escape m.username) action_btn
  in

  let render_mod_row (m : moderator_entry) =
    let action_btns =
      if can_manage then
        Printf.sprintf "
          <div class='flex items-center gap-2'>
            <form action='/c/%s/manage-mods/promote' method='POST' class='inline m-0 p-0'>
              %s
              <input type='hidden' name='target_user_id' value='%d'>
              <button type='submit' class='text-xs text-[#69C3D2] hover:text-[#4BA8B8] font-bold border border-[#A8DDE8] px-2 py-1 rounded transition'>Promote to Top Mod</button>
            </form>
            <form action='/c/%s/manage-mods/remove' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Remove this moderator?')\">
              %s
              <input type='hidden' name='target_user_id' value='%d'>
              <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold border border-red-200 px-2 py-1 rounded transition'>Remove</button>
            </form>
          </div>"
          (Components.html_escape community.slug) csrf_token m.user_id
          (Components.html_escape community.slug) csrf_token m.user_id
      else ""
    in
    Printf.sprintf "
      <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
        <a href='/u/%s' class='font-medium text-gray-800 hover:text-[#C94C4C] hover:underline'>u/%s</a>
        %s
      </li>"
      (Components.html_escape m.username) (Components.html_escape m.username) action_btns
  in

  let render_legacy_row (m : moderator_entry) =
    Printf.sprintf "
      <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
        <div><a href='/u/%s' class='font-medium text-gray-500 hover:text-[#C94C4C] hover:underline'>u/%s</a>
        <span class='ml-2 text-xs bg-gray-100 text-gray-500 px-1.5 py-0.5 rounded'>Legacy</span></div>
        <span class='text-xs text-gray-400 italic'>No active powers</span>
      </li>"
      (Components.html_escape m.username) (Components.html_escape m.username)
  in

  let top_mod_section =
    if top_mods = [] then "<p class='text-gray-400 italic text-xs'>No Top Mods yet.</p>"
    else Printf.sprintf "<ul>%s</ul>" (String.concat "\n" (List.map render_top_mod_row top_mods))
  in
  let mod_section =
    if regular_mods = [] then "<p class='text-gray-400 italic text-xs'>No standard moderators.</p>"
    else Printf.sprintf "<ul>%s</ul>" (String.concat "\n" (List.map render_mod_row regular_mods))
  in
  let legacy_section =
    if legacy_mods = [] then ""
    else Printf.sprintf "
      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-1'>Legacy Moderators</h2>
        <p class='text-xs text-gray-400 mb-4'>Demoted due to inactivity. No permissions granted.</p>
        <ul>%s</ul>
      </div>" (String.concat "\n" (List.map render_legacy_row legacy_mods))
  in

  let add_mod_form =
    if can_manage then Printf.sprintf "
      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Add New Moderator</h2>
        <form action='/c/%s/manage-mods/add' method='POST' class='flex items-center space-x-3'>
          %s
          <input type='text' name='username' required placeholder='Username'
                 class='flex-1 rounded-xl border-[#D0C9BC] shadow-sm focus:border-[#C94C4C] p-2 border text-sm'>
          <button type='submit' class='bg-[#C94C4C] text-white px-4 py-2 rounded font-bold text-sm hover:bg-[#A83A3A] transition shadow-sm'>
            Add Mod
          </button>
        </form>
      </div>" (Components.html_escape community.slug) csrf_token
    else ""
  in

  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto'>
      <div class='flex items-center justify-between mb-6'>
        <h1 class='text-2xl font-bold text-gray-900'>Council of Mods &mdash; /c/%s</h1>
        <a href='/c/%s' class='text-sm text-[#C94C4C] hover:underline'>&larr; Back to Community</a>
      </div>

      %s

      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-1'>Top Mods</h2>
        <p class='text-xs text-gray-400 mb-4'>Council seats (max 3). Only admins can remove Top Mods.</p>
        %s
      </div>

      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Moderators</h2>
        %s
      </div>

      %s
    </div>"
    (Components.html_escape community.slug) (Components.html_escape community.slug)
    add_mod_form
    top_mod_section
    mod_section
    legacy_section
  in
  Components.layout ?user ~request ~title:(Printf.sprintf "Manage Mods — /c/%s" community.slug) content

(* === POST === *)

let choose_community_page ?user (communities : community list) =
  let render_option (community : community) =
    Printf.sprintf "
    <a href='/new-post?community=%s' class='block bg-white p-4 rounded-xl border border-[#E0D9CC] hover:border-[#C94C4C] hover:shadow-md transition flex justify-between items-center group'>
        <div>
            <span class='font-bold text-lg text-gray-800 group-hover:text-[#C94C4C]'>%s</span>
            <span class='text-sm text-gray-500 ml-2'>/c/%s</span>
        </div>
        <div class='bg-gray-100 text-gray-600 px-3 py-1 rounded text-sm group-hover:bg-[#C94C4C] group-hover:text-white transition'>
            Post here &rarr;
        </div>
    </a>"
    community.slug community.name community.slug
  in

  let list_html = String.concat "\n" (List.map render_option communities) in

  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800'>Choose a Community</h1>
        <p class='text-gray-600 mb-6'>Select where you want to publish your post:</p>

        <div class='space-y-3'>
            %s
        </div>

        <div class='mt-8 text-center pt-6 border-t'>
            <p class='text-gray-500'>Can't find the right place?</p>
            <a href='/new-community' class='text-[#C94C4C] font-bold hover:underline'>Create a new Community</a>
        </div>
    </div>"
    list_html
  in
  Components.layout ?user ~title:"Choose Community" content

let join_to_post_page ?user (community : community) request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto mt-10 p-8 bg-white rounded-xl shadow-md border border-[#E0D9CC] text-center'>
        <div class='text-6xl mb-4'>🔒</div>
        <h1 class='text-2xl font-bold text-gray-800 mb-2'>Members Only</h1>
        <p class='text-gray-600 mb-6'>
            You must be a member of <span class='font-bold text-[#C94C4C]'>/c/%s</span> to create a post here.
        </p>

        <form action='/join' method='POST'>
            %s
            <input type='hidden' name='community_id' value='%d'>
            <input type='hidden' name='redirect_to' value='/new-post?community=%s'>

            <button type='submit' class='w-full bg-[#C94C4C] text-white font-bold py-3 px-4 rounded-xl hover:bg-[#A83A3A] transition shadow-lg transform hover:-translate-y-0.5'>
                Join Community & Post
            </button>
        </form>

        <div class='mt-4'>
            <a href='/' class='text-sm text-gray-500 hover:underline'>Cancel and return Home</a>
        </div>
    </div>"
    community.slug csrf_token community.id community.slug
  in
  Components.layout ?user ~request ~title:("Join " ^ community.name) content

let new_post_form ?user (community : community) request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-xl shadow-md border border-[#E0D9CC]'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800'>Post to <span class='text-[#C94C4C]'>/c/%s</span></h1>

        <form action='/posts' method='POST' enctype='multipart/form-data' class='space-y-6'>
            %s
            <input type='hidden' name='community_id' value='%d'>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Title</label>
                <input type='text' name='title' required
                       class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='An interesting title'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>URL (Optional)</label>
                <input type='url' name='url'
                       class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                       placeholder='https://example.com'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Image (Optional, max 5 MB)</label>
                <input type='file' name='image' accept='image/*'
                       class='block w-full text-sm text-gray-500 file:mr-4 file:py-2 file:px-4 file:rounded-xl file:border-0 file:text-sm file:font-semibold file:bg-[#F7F0E8] file:text-[#C94C4C] hover:file:bg-[#EDE5D8]'>
                <p class='text-xs text-gray-400 mt-1'>Converted to WebP automatically. Leave blank for a text or link post.</p>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Text (Optional)</label>
                <textarea name='content' rows='6'
                          class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'
                          placeholder='Share your thoughts...'></textarea>
            </div>

            <button type='submit' class='w-full bg-[#C94C4C] text-white py-2 px-4 rounded-xl hover:bg-[#A83A3A] font-semibold transition'>
                Submit Post
            </button>
        </form>
    </div>"
    community.slug
    csrf_token
    community.id
  in
  Components.layout ?user ~request ~title:("Post to " ^ community.name) content

let post_page ?user ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ~community ~user_communities ~moderated_communities user_post_votes user_comment_votes (post : post) (comments : comment list) request =
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
                    <input type='hidden' name='community_id' value='%d'>
                    <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold'>🗑️</button>
                </form>" csrf_token c.id post.community_id
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

        let avatar_html = match c.avatar_url with
          | Some url when url <> "" -> Printf.sprintf "<img src='%s' alt='Avatar' class='w-5 h-5 rounded-full object-cover shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC]'>" url
          | _ -> Printf.sprintf "<div class='w-5 h-5 bg-[#DFF5F8] rounded-full flex items-center justify-center text-[10px] text-[#C94C4C] font-bold shadow-sm border border-[#A8DDE8]'>%s</div>" (String.sub c.username 0 1 |> String.uppercase_ascii)
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

        Printf.sprintf "
        <div class='mt-3 hover:bg-[#EDE9DF] transition rounded-r pr-2 py-1'>
            <div class='flex items-center gap-2 text-xs text-gray-500 mb-1'>
                %s
                %s
                %s%s
                <span class='text-gray-400'>•</span>
                <span class='text-gray-400'>%s</span>
                %s
                %s
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

  let post_content = match post.content with | Some c -> Printf.sprintf "<div class='text-sm text-gray-800 leading-relaxed whitespace-pre-wrap break-words mt-2 mb-3'>%s</div>" (Components.html_escape c) | None -> "" in
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

  let content = Printf.sprintf "
    <div class='flex flex-col lg:flex-row gap-6 items-start'>
        <div class='w-full lg:w-1/4 hidden lg:block self-start sticky top-20'>
            %s
        </div>
        <div class='w-full lg:w-2/4 min-w-0'>
            <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-6 mb-6'>
                <div class='mb-4'>
                    <a href='/c/%s' class='text-sm font-bold text-[#C94C4C] hover:underline'>/c/%s</a>
                    <span class='text-gray-400 mx-2'>•</span>
                    <span class='text-sm text-gray-500'>Posted by %s</span>
                    <span class='text-gray-400 mx-1'>•</span>
                    <span class='text-sm text-gray-500'>%s</span>
                    %s
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
    (Components.left_sidebar ?user ~moderated_communities user_communities)
    post.community_slug post.community_slug
    (Components.render_author ~mod_usernames ~admin_usernames post.username) (Components.time_ago post.created_at) (post_action_btn ^ ban_post_btn)
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
  Components.layout ?user ~request ~title:post.title (toggle_script ^ content)

(* === USER === *)

let user_profile_page ?user ~is_admin ~is_globally_banned ~profile_id ~admin_usernames ~moderated_communities ~active_tab user_votes username joined_at bio_opt avatar_url_opt karma posts user_comments request =
  let csrf_token = Dream.csrf_tag request in
  let bio = Option.value ~default:"This user hasn't written a bio yet." bio_opt in
  let avatar_url = Option.value ~default:"https://www.gravatar.com/avatar/00000000000000000000000000000000?d=mp&f=y" avatar_url_opt in

  (* Profile role badges: ADMIN badge driven by admin_usernames (already fetched),
     MOD badges iterate moderated_communities — one badge per community the user moderates. *)
  let profile_admin_badge =
    if List.mem username admin_usernames then
      "<span class='text-[11px] font-bold bg-red-100 text-red-700 px-2 py-0.5 rounded'>[ADMIN]</span>"
    else ""
  in
  let mod_badges =
    String.concat " " (List.map (fun (a : community) ->
      Printf.sprintf "<a href='/c/%s' class='text-[11px] font-bold bg-green-100 text-green-700 px-2 py-0.5 rounded hover:bg-green-200 transition'>[MOD of /c/%s]</a>"
        a.slug a.slug
    ) moderated_communities)
  in
  (* Banned badge: shown on the profile header so any visitor sees the account status.
     Admin controls: Ban ↔ Unban toggle driven by is_globally_banned to prevent
     double-ban confusion and surface the current state clearly. *)
  let globally_banned_badge =
    if is_globally_banned then
      "<span class='text-[11px] font-bold bg-gray-800 text-white px-2 py-0.5 rounded'>🚫 GLOBALLY BANNED</span>"
    else ""
  in
  let role_badges =
    if profile_admin_badge = "" && mod_badges = "" && globally_banned_badge = "" then ""
    else Printf.sprintf "<div class='flex flex-wrap items-center gap-1.5 mt-2'>%s%s%s</div>" profile_admin_badge mod_badges globally_banned_badge
  in
  (* Edit Profile: only render when the logged-in user is viewing their own profile.
     Avoids exposing /settings entry point on others' profiles — a cosmetic boundary
     that reinforces the expectation that settings are personal. *)
  let edit_profile_btn =
    match user with
    | Some logged_in when logged_in = username ->
      "<a href='/settings' class='inline-flex items-center gap-1.5 mt-3 bg-[#C94C4C] text-white text-sm font-semibold px-4 py-1.5 rounded-xl hover:bg-[#A83A3A] transition shadow-sm'>✏️ Edit Profile</a>"
    | _ -> ""
  in
  (* Admin Panel: only render for the logged-in admin on their own profile.
     Gating on both own-profile AND is_admin prevents leaking the /admin route
     to non-admins even if they inspect another admin's profile page. *)
  let admin_panel_btn =
    match user with
    | Some logged_in when logged_in = username && is_admin ->
      "<a href='/admin' class='inline-flex items-center gap-1.5 mt-3 bg-red-600 text-white text-sm font-semibold px-4 py-1.5 rounded-xl hover:bg-red-700 transition shadow-sm'>🛡️ Admin Panel</a>"
    | _ -> ""
  in

  let admin_controls =
    if is_admin && (Option.value ~default:"" user) <> username then
      let ban_or_unban_btn =
        if is_globally_banned then
          Printf.sprintf "
            <form action='/admin/unban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">
                %s
                <button type='submit' class='bg-green-600 text-white text-xs font-bold py-2 px-4 rounded hover:bg-green-700 shadow-sm transition'>
                    ✅ UNBAN USER
                </button>
            </form>" profile_id username csrf_token
        else
          Printf.sprintf "
            <form action='/admin/ban/user/%d' method='POST' onsubmit=\"confirmModal(event, 'Permanently ban u/%s? They will be blocked from logging in and posting.')\">
                %s
                <button type='submit' class='bg-red-600 text-white text-xs font-bold py-2 px-4 rounded hover:bg-red-700 shadow-sm transition'>
                    🔨 BAN USER
                </button>
            </form>" profile_id username csrf_token
      in
      Printf.sprintf "
        <div class='mt-6 pt-4 border-t border-red-200'>
            <h3 class='text-xs font-bold text-red-700 mb-2 uppercase tracking-wide'>🛡️ Admin Controls</h3>
            %s
        </div>" ban_or_unban_btn
    else ""
  in

  let tab_class active =
    if active_tab = active then
      "text-[#C94C4C] border-b-2 border-[#C94C4C] pb-3 text-sm font-medium"
    else
      "text-gray-500 hover:text-gray-700 border-b-2 border-transparent hover:border-[#D0C9BC] pb-3 text-sm font-medium transition-colors"
  in
  let tab_nav = Printf.sprintf "
    <div class='flex items-center gap-6 border-b border-[#E0D9CC] mb-6'>
        <a href='/u/%s?tab=posts' class='%s'>Posts</a>
        <a href='/u/%s?tab=comments' class='%s'>Comments</a>
    </div>" username (tab_class "posts") username (tab_class "comments")
  in

  let posts_html =
    if posts = [] then
      "<p class='text-gray-500 italic mt-4'>This user hasn't posted anything yet.</p>"
    else
      String.concat "\n" (List.map (Components.render_post ~admin_usernames request user_votes) posts)
  in

  let comments_html =
    if user_comments = [] then
      "<p class='text-gray-500 italic mt-4'>This user hasn't commented anything yet.</p>"
    else
      String.concat "\n" (List.map (fun (_id, content, created_at, post_id, post_title, score) ->
        Printf.sprintf "
          <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm p-4 mb-4'>
              <div class='flex items-center gap-3 text-xs text-gray-400 mb-2'>
                  <span>%s</span>
                  <span class='text-gray-300'>·</span>
                  <span class='font-medium text-gray-500'>%d points</span>
              </div>
              <p class='text-sm text-gray-800 leading-relaxed'>%s</p>
              <a href='/p/%d' class='text-xs text-[#C94C4C] hover:underline mt-2 inline-block'>&#8618; Commented on: %s</a>
          </div>" created_at score content post_id post_title
      ) user_comments)
  in

  let feed_html =
    if active_tab = "comments" then comments_html else posts_html
  in

  let content = Printf.sprintf "
    <div class='max-w-4xl mx-auto mt-8'>
        <div class='bg-white p-8 rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] mb-8'>
            <div class='flex items-center space-x-6'>
                <img src='%s' alt='%s avatar' class='w-24 h-24 rounded-full border-4 border-gray-50 object-cover shadow-sm'>
                <div>
                    <h1 class='text-3xl font-bold text-gray-900'>u/%s</h1>
                    %s
                    <div class='flex items-center gap-2 flex-wrap'>%s%s</div>
                    <div class='flex items-center space-x-4 mt-2 text-sm text-gray-600'>
                        <span><strong class='text-gray-900'>%d</strong> Karma</span>
                        <span>Joined %s</span>
                    </div>
                </div>
            </div>

            <div class='mt-6 bg-[#EDE9DF] p-4 rounded-xl border border-[#E0D9CC] text-gray-800 whitespace-pre-wrap text-sm'>%s</div>

            %s </div>

        %s
        <div>
            %s
        </div>
    </div>" avatar_url username username role_badges edit_profile_btn admin_panel_btn karma joined_at bio admin_controls tab_nav feed_html
  in
  Components.layout ?user ~request ~title:(username ^ "'s Profile") content

let settings_page ?user bio avatar_url request =
  let csrf_token = Dream.csrf_tag request in
  let current_bio = Option.value ~default:"" bio in
  let current_avatar = Option.value ~default:"" avatar_url in

  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] mt-8'>
        <h1 class='text-3xl font-extrabold text-gray-900 mb-8'>Account Settings</h1>

        <div class='mb-10 pb-8 border-b border-[#E0D9CC]'>
            <h2 class='text-xl font-bold text-gray-800 mb-4'>Profile Information</h2>
            <form action='/settings' method='POST' enctype='multipart/form-data' class='space-y-6'>
                %s
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Avatar Image (Optional)</label>
                    <input type='hidden' name='existing_avatar_url' value='%s'>
                    <input type='file' name='avatar_url' accept='image/*' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Bio</label>
                    <textarea name='bio' rows='4' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border' placeholder='Tell the community a bit about yourself...'>%s</textarea>
                </div>
                <button type='submit' class='bg-[#C94C4C] text-white font-bold py-2 px-6 rounded hover:bg-[#A83A3A] transition shadow-sm'>Save Profile</button>
            </form>
        </div>

        <div class='mb-10 pb-8 border-b border-[#E0D9CC]'>
            <h2 class='text-xl font-bold text-gray-800 mb-4'>Change Password</h2>
            <form action='/settings/password' method='POST' class='space-y-4'>
                %s
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Current Password</label>
                    <input type='password' name='old_password' required class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>New Password</label>
                    <input type='password' name='new_password' required minlength='8' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Confirm New Password</label>
                    <input type='password' name='confirm_password' required minlength='8' class='block w-full rounded-xl border-[#D0C9BC] focus:border-[#C94C4C] focus:ring-1 focus:ring-[#C94C4C]/20 focus:outline-none p-2 border'>
                </div>
                <button type='submit' class='bg-gray-800 text-white font-bold py-2 px-6 rounded hover:bg-gray-900 transition shadow-sm'>Update Password</button>
            </form>
        </div>

        <div class='mb-10 pb-8 border-b border-[#E0D9CC]'>
            <h2 class='text-xl font-bold text-gray-900 mb-2'>Data Portability (GDPR Art. 20)</h2>
            <p class='text-gray-600 text-sm mb-4'>Download a complete machine-readable copy (JSON) of your personal data, including your profile information, posts, and comments.</p>
            <a href='/export-data' class='inline-block bg-[#F0EDE4] text-[#69C3D2] border border-[#A8DDE8] font-bold py-2 px-6 rounded hover:bg-[#DFF5F8] transition shadow-sm'>↓ Download My Data</a>
        </div>

        <div class='mt-8 pt-6 border border-red-200 bg-red-50 p-6 rounded-xl'>
            <h2 class='text-xl font-bold text-red-700 mb-2'>Danger Zone</h2>
            <p class='text-red-600 text-sm mb-6'>Permanently delete your account and personal data. Your posts and comments will remain, but their author will be anonymized as <strong>[deleted]</strong>. This action is irreversible.</p>
            <form action='/delete-account' method='POST' onsubmit=\"confirmModal(event, 'WARNING: Permanently delete your personal data? This action is irreversible.')\">
                %s
                <button type='submit' class='bg-red-600 text-white font-bold py-2 px-6 rounded hover:bg-red-700 transition shadow-sm'>Delete Account</button>
            </form>
        </div>
    </div>"
    csrf_token current_avatar current_bio csrf_token csrf_token
  in
  Components.layout ~noindex:true ?user ~request ~title:"Settings" content

let notifications_page ?user (notifs : Db.notification list) request =
  let render_notif (n : Db.notification) =
    let bg_color = if n.is_read then "bg-white" else "bg-[#F0EDE4] border-l-4 border-[#C94C4C]" in
    let icon = match n.notif_type with
      | "mention"    -> "&#64;"   (* @ symbol — avoids mojibake in Printf *)
      | "mod_action" -> "&#9888;" (* ⚠ warning sign *)
      | _ ->
          (* Legacy comment_reply: distinguish post vs comment reply by message suffix. *)
          let len = String.length n.message in
          if len >= 5 && String.sub n.message (len - 5) 5 = "post." then "&#128221;" (* 📝 *)
          else "&#128172;" (* 💬 *)
    in
    let inner = Printf.sprintf "
        <div class='flex items-center'>
            <div class='text-2xl mr-4'>%s</div>
            <div>
                <p class='text-gray-900 font-medium'>%s</p>
                <p class='text-xs text-gray-500 mt-1'>%s</p>
            </div>
        </div>" icon (Components.html_escape n.message) (Components.time_ago n.created_at)
    in
    (* Mod-action notifications without a post link render as non-clickable divs. *)
    match n.post_id with
    | Some pid ->
        Printf.sprintf "
    <a href='/p/%d' onclick=\"this.classList.remove('bg-[#F0EDE4]','border-l-4','border-[#C94C4C]');this.classList.add('bg-white');\" class='block %s p-4 rounded-xl border border-[#E0D9CC] hover:shadow-md transition mb-3'>%s
    </a>" pid bg_color inner
    | None ->
        Printf.sprintf "
    <div class='block %s p-4 rounded-xl border border-[#E0D9CC] mb-3'>%s
    </div>" bg_color inner
  in
  let list_html =
    if notifs = [] then "<div class='text-center py-12 text-gray-500 bg-white rounded border border-dashed'>No notifications yet.</div>"
    else String.concat "\n" (List.map render_notif notifs)
  in
  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto'>
        <h1 class='text-3xl font-extrabold text-gray-900 mb-6'>Notifications</h1>
        %s
    </div>" list_html
  in Components.layout ~noindex:true ?user ~request ~title:"Notifications" content

(* === SEARCH === *)

let search_results_page ?user ~admin_usernames user_votes current_page active_tab query (communities: community list) users (posts: post list) comments request =
  (* Escape the search query once — used in HTML text, title, and href attributes.
     HTML-encoding in href is correct: browsers decode entities before navigation. *)
  let query = Components.html_escape query in

  let render_community (a: community) =
    Printf.sprintf "<a href='/c/%s' class='block p-4 bg-white border border-[#E0D9CC] rounded-xl hover:border-[#C94C4C] mb-3 shadow-sm'><h3 class='font-bold text-lg text-gray-900'>%s</h3><p class='text-xs text-[#C94C4C] mb-1'>/c/%s</p><p class='text-gray-700 text-sm'>%s</p></a>"
      (Components.html_escape a.slug) (Components.html_escape a.name)
      (Components.html_escape a.slug) (Components.html_escape (Option.value ~default:"No description" a.description))
  in

  let render_user (_, username, _, bio, avatar) =
    let eu = Components.html_escape username in
    let avatar_html = match avatar with
      | Some url when url <> "" -> Printf.sprintf "<img src='%s' class='w-12 h-12 rounded-full object-cover mr-4'>" (Components.html_escape url)
      | _ -> Printf.sprintf "<div class='w-12 h-12 bg-[#DFF5F8] rounded-full flex items-center justify-center text-[#C94C4C] font-bold mr-4'>%s</div>" (String.sub username 0 1 |> String.uppercase_ascii)
    in
    Printf.sprintf "<a href='/u/%s' class='block p-4 bg-white border border-[#E0D9CC] rounded-xl hover:border-[#C94C4C] mb-3 shadow-sm flex items-center'>%s <div><h3 class='font-bold text-lg text-gray-900'>u/%s</h3><p class='text-sm text-gray-500'>%s</p></div></a>"
      eu avatar_html eu (Components.html_escape (Option.value ~default:"" bio))
  in

  let render_search_comment (_, content, username, created_at, post_id, score) =
    Printf.sprintf "<div class='p-4 bg-white border border-[#E0D9CC] rounded-xl mb-3 shadow-sm'><div class='text-xs text-gray-500 mb-2'>%s • %s • Score: %d</div><div class='text-gray-800 text-sm mb-3'>%s</div><a href='/p/%d' class='text-xs font-bold text-[#C94C4C] hover:underline bg-[#F0EDE4] px-2 py-1 rounded'>Go to Thread &rarr;</a></div>"
      (Components.render_author ~admin_usernames username) (Components.time_ago created_at) score (Components.html_escape content) post_id
  in

  let content_html, has_next =
    match active_tab with
    | "communities" ->
        if communities = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-xl border border-dashed border-[#D0C9BC]'>No communities found for this search.</div>", false)
        else (String.concat "\n" (List.map render_community communities), List.length communities = 20)
    | "people" ->
        if users = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-xl border border-dashed border-[#D0C9BC]'>No people found for this search.</div>", false)
        else (String.concat "\n" (List.map render_user users), List.length users = 20)
    | "comments" ->
        if comments = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-xl border border-dashed border-[#D0C9BC]'>No comments found for this search.</div>", false)
        else (String.concat "\n" (List.map render_search_comment comments), List.length comments = 20)
    | _ ->
        if posts = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-xl border border-dashed border-[#D0C9BC]'>No posts found for this search.</div>", false)
        else (String.concat "\n" (List.map (Components.render_post ~admin_usernames request user_votes) posts), List.length posts = 20)
  in

  let get_tab_class tab_name =
    if tab_name = active_tab then
      "font-bold text-sm tracking-wide uppercase text-[#C94C4C] pb-3 border-b-2 border-[#C94C4C]"
    else
      "font-bold text-sm tracking-wide uppercase text-gray-500 hover:text-[#C94C4C] pb-3 border-b-2 border-transparent hover:border-[#C94C4C] transition"
  in

  let tabs_html = Printf.sprintf "
    <div class='bg-gray-50 pt-4 mb-6 border-b border-[#E0D9CC] flex space-x-8 overflow-x-auto'>
        <a href='/search?q=%s&t=posts' class='%s'>Posts</a>
        <a href='/search?q=%s&t=communities' class='%s'>Communities</a>
        <a href='/search?q=%s&t=comments' class='%s'>Comments</a>
        <a href='/search?q=%s&t=people' class='%s'>People</a>
    </div>"
    query (get_tab_class "posts")
    query (get_tab_class "communities")
    query (get_tab_class "comments")
    query (get_tab_class "people")
  in

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='/search?q=%s&t=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50'>&larr; Prev</a>" query active_tab (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='/search?q=%s&t=%s&page=%d' class='bg-white border border-[#D0C9BC] text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50'>Next &rarr;</a>" query active_tab (current_page + 1) in

  let content = Printf.sprintf "
    <div class='max-w-5xl mx-auto'>
        <div class='mb-2'><h1 class='text-2xl font-bold text-gray-900'>Search results for \"%s\"</h1></div>

        %s <div class='space-y-4'>
            %s </div>

        <div class='flex justify-between items-center mt-8 mb-4'>
            <div>%s</div>
            <div>%s</div>
        </div>
    </div>" query tabs_html content_html prev_btn next_btn
  in
  Components.layout ?user ~request ~title:("Search: " ^ query) content

(* === LEGAL / PRIVACY === *)

(* Single-section structure: plain-English human summary up top, then technical spec.
   Grounded in actual schema/auth.ml — no invented infrastructure or fictional DPO. *)
let privacy_page ?user request =
  let content = "
    <div class='max-w-2xl mx-auto mt-10 mb-16'>

      <h1 class='text-3xl font-extrabold text-gray-900 mb-2'>Privacy Policy</h1>
      <p class='text-sm text-gray-400 mb-10'>Last updated: March 2026 &mdash; Written by Dami</p>

      <!-- Short version: intentionally blunt, no corporate hedging. -->
      <div class='bg-[#F0FDF9] border border-[#C94C4C]/20 rounded-xl p-6 mb-12'>
        <p class='text-xs font-bold text-[#C94C4C] uppercase tracking-widest mb-4'>The Short Version (Plain English)</p>
        <div class='space-y-4 text-gray-800 leading-relaxed'>
          <p class='text-base font-semibold'>Everything Earde collects about you:</p>
          <ul class='list-none space-y-2 text-sm'>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Your username.</strong> Public. Your identity on the platform.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Your email address.</strong> Private. Used only to verify your account and send password resets.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Your password &mdash; hashed, immediately.</strong> The raw password is discarded the instant it is received. Only an Argon2id hash is stored. We cannot recover or read your password.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Your posts, comments, and votes.</strong> The content you choose to contribute.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Your IP address, for security.</strong> Temporarily stored in the database to enforce rate limiting on login and signup. Not used for anything else.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#C94C4C] font-bold mt-0.5'>&#10003;</span><span><strong>Anonymous daily page-view counts.</strong> We count visits per page using a daily-resetting hash of your IP, browser string, and the current date. The hash is one-way: the original IP cannot be recovered from it.</span></li>
          </ul>
          <div class='border-t border-[#C94C4C]/20 pt-4 mt-4 space-y-2 text-sm text-gray-700'>
            <p><strong>No advertising. No tracking pixels. No Google Analytics. No Meta Pixel. No data brokers. No behavioural profiling. No selling your data.</strong></p>
            <p>We set exactly one cookie: a session cookie that keeps you logged in. It is deleted when you log out. No cookie banner is shown because this cookie is strictly necessary for the service to function, so consent is not legally required for it.</p>
            <p>You can <a href='/export-data' class='text-[#C94C4C] underline hover:text-[#A83A3A] font-medium'>download all your data</a> as JSON at any time. You can <a href='/settings' class='text-[#C94C4C] underline hover:text-[#A83A3A] font-medium'>delete your account</a> instantly from Settings.</p>
          </div>
        </div>
      </div>

      <p class='text-xs font-bold text-gray-500 uppercase tracking-widest mb-6'>Technical Specification</p>

      <div class='space-y-10 text-gray-700 leading-relaxed'>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>1. Who I Am</h2>
          <p class='mb-3'>I am Dami, a developer based in Italy. I built and run Earde alone. There is no company, no legal team, no DPO, and no dedicated privacy email address, just me. Contact me for all data protection matters: <a href='mailto:dami@earde.com' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>dami@earde.com</a>.</p>
          <p class='mb-3'>This policy reflects the actual source code.  If anything here contradicts the code, the code is the ground truth, and I want to know so I can fix the policy.</p>
          <ul class='list-disc list-inside space-y-2 text-sm mt-3'>
            <li><strong>Earde is fully open-source.</strong> You do not have to trust this policy. Read the code and verify it yourself: <a href='https://github.com/earde-social/earde' class='text-[#C94C4C] underline hover:text-[#A83A3A]' target='_blank' rel='noopener noreferrer'>https://github.com/earde-social/earde</a></li>
            <li>I collect the minimum data needed to run this.</li>
            <li>No third-party services receive your data. Your browser makes no requests to any external server when you use Earde.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>2. Infrastructure &amp; Hosting</h2>
          <p class='mb-3'>Earde runs on servers provided by <strong>Hetzner</strong>, a German hosting company. All servers are located physically inside the European Union, either in Germany (Nuremberg/Falkenstein) or Finland (Helsinki).</p>
          <p class='mb-3'>Your data never leaves EU jurisdiction. I do not use Amazon Web Services, Google Cloud, Microsoft Azure, or any comparable US-based cloud platform. There is no CDN routing your traffic through non-EU nodes. The infrastructure is a bespoke setup (server, PostgreSQL database, application) with no intermediary black boxes. The entire data pipeline is subject to EU law, including the GDPR, from end to end.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>3. Data Collected &amp; Legal Basis (Art. 6)</h2>
          <p class='mb-4 text-sm'>Every category of personal data, its exact storage location, retention period, and GDPR legal basis:</p>
          <div class='overflow-x-auto'>
            <table class='w-full text-sm border border-[#E0D9CC] rounded-xl overflow-hidden'>
              <thead class='bg-gray-50 text-gray-600 font-semibold'>
                <tr>
                  <th class='text-left px-4 py-2 border-b border-[#E0D9CC]'>Data</th>
                  <th class='text-left px-4 py-2 border-b border-[#E0D9CC]'>Storage</th>
                  <th class='text-left px-4 py-2 border-b border-[#E0D9CC]'>Retention</th>
                  <th class='text-left px-4 py-2 border-b border-[#E0D9CC]'>Basis</th>
                </tr>
              </thead>
              <tbody class='divide-y divide-gray-100'>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Username</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>users.username</code> &mdash; plaintext, public</td>
                  <td class='px-4 py-2 text-gray-500'>Until deletion; replaced with <code class='bg-gray-100 px-1 rounded'>[deleted_N]</code></td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Email address</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>users.email</code> &mdash; plaintext, never shown in UI</td>
                  <td class='px-4 py-2 text-gray-500'>Until deletion; overwritten with <code class='bg-gray-100 px-1 rounded'>deleted_N@earde.local</code></td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Password hash</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>users.password_hash</code> &mdash; Argon2id encoded string; raw password never stored</td>
                  <td class='px-4 py-2 text-gray-500'>Until deletion; zeroed</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Posts &amp; comments</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>posts</code>, <code class='bg-gray-100 px-1 rounded'>comments</code> tables</td>
                  <td class='px-4 py-2 text-gray-500'>Indefinite as community content; authorship tombstoned on deletion (see &sect;6)</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Votes</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>post_votes</code>, <code class='bg-gray-100 px-1 rounded'>comment_votes</code> tables</td>
                  <td class='px-4 py-2 text-gray-500'>Hard-deleted on account deletion (CASCADE)</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Bio &amp; avatar (optional)</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>users.bio</code>, <code class='bg-gray-100 px-1 rounded'>users.avatar_url</code></td>
                  <td class='px-4 py-2 text-gray-500'>Until changed or account deletion</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(a) &mdash; consent</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>IP address (rate limiting)</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>rate_limits.ip_address</code> &mdash; plaintext, alongside attempt count and time window</td>
                  <td class='px-4 py-2 text-gray-500'>Sliding window (~60s); stale rows are overwritten, not archived</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(f) &mdash; legitimate interest (security)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Page-view session hash</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>page_views.session_hash</code> &mdash; MD5(IP + User-Agent + date); one-way, non-reversible</td>
                  <td class='px-4 py-2 text-gray-500'>Indefinite aggregate; hash resets every 24 hours by design</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(f) &mdash; legitimate interest (understanding usage)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Password reset token</td>
                  <td class='px-4 py-2 text-gray-500'><code class='bg-gray-100 px-1 rounded'>password_resets.token</code></td>
                  <td class='px-4 py-2 text-gray-500'>Hard expiry via <code class='bg-gray-100 px-1 rounded'>expires_at</code>; deleted on use (CASCADE on user deletion)</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b)</td>
                </tr>
              </tbody>
            </table>
          </div>
          <p class='mt-4 text-sm text-gray-500'>No real name, date of birth, phone number, or any other identifying information is collected or requested. No special-category data (Art. 9). No data transfers outside the EEA. No automated decision-making or profiling (Art. 22).</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>4. Password Security (Art. 32)</h2>
          <p class='mb-3'>When you set a password, the server runs the following logic (from <code class='bg-gray-100 px-1 rounded text-sm'>auth.ml</code>):</p>
          <pre class='bg-gray-900 text-green-300 rounded-xl p-4 text-sm overflow-x-auto leading-relaxed'>let salt = Dream.random 16  (* 16 bytes from the framework CSPRNG *)
Argon2.hash
  ~t_cost:2           (* 2 iterations *)
  ~m_cost:65536       (* 64 MB of RAM required per hash attempt *)
  ~parallelism:1
  ~kind:Argon2.ID     (* Argon2id variant *)
  ~version:Argon2.VERSION_13
  ~hash_len:32
  ...</pre>
          <p class='mt-3 mb-3'><strong>Argon2id</strong> is the algorithm recommended by OWASP and selected by the Password Hashing Competition. The <code class='bg-gray-100 px-1 rounded text-sm'>ID</code> variant provides resistance against both side-channel and GPU brute-force attacks. The 64 MB memory requirement means cracking a single password requires 64 MB of RAM per attempt, making large-scale GPU attacks prohibitive.</p>
          <p>A fresh 16-byte random salt is generated for every password, so two users with identical passwords will have completely different hashes. Only the encoded hash string is written to the database. The plaintext password is never stored, logged, or accessible after the request completes.</p>
          <p class='mt-3 text-sm text-gray-500'>Additional security measures: CSRF tokens on all state-mutating forms; HTML output escaping preventing XSS; rate limiting on auth endpoints; session cookie with <code class='bg-gray-100 px-1 rounded'>Secure</code>, <code class='bg-gray-100 px-1 rounded'>HttpOnly</code>, and <code class='bg-gray-100 px-1 rounded'>SameSite=Strict</code> attributes.</p>
        </section>

        <!-- ePrivacy Directive 2002/58/EC, Recital 25 -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>5. Cookies &amp; Tracking</h2>
          <p class='mb-3'>Earde sets <strong>exactly one cookie</strong>: a session cookie issued by the Dream web framework. It carries only an opaque session identifier. The session data (user ID, username) is stored server-side in the <code class='bg-gray-100 px-1 rounded text-sm'>dream_session</code> PostgreSQL table, nothing personal is embedded in the cookie itself.</p>
          <p class='mb-3'>This cookie is strictly necessary for the service to function. Under ePrivacy Directive 2002/58/EC, Art. 5(3) and Recital 25, strictly-necessary cookies are exempt from prior-consent requirements. This is why there is no cookie consent banner.</p>
          <p class='mb-3'>The cookie is cleared immediately on logout. It does not persist across devices or browser profiles.</p>
          <p class='font-medium text-gray-900'>What we do <em>not</em> use:</p>
          <ul class='list-disc list-inside space-y-1 text-sm mt-2'>
            <li>Google Analytics, Google Tag Manager, or any Google product</li>
            <li>Meta (Facebook) Pixel or any Meta tracking</li>
            <li>Any third-party advertising or analytics script</li>
            <li>Cross-site tracking or browser fingerprinting</li>
          </ul>
          <p class='mt-3 text-sm'>When you visit Earde, your browser makes no requests to any third-party server.</p>
        </section>

        <!-- Art. 17 GDPR: right to erasure — exact SQL from the codebase -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>6. Account Deletion &amp; Right to Erasure (Art. 17)</h2>
          <p class='mb-3'>When you delete your account, the server calls <code class='bg-gray-100 px-1 rounded text-sm'>Db.anonymize_user</code>, which executes this SQL:</p>
          <pre class='bg-gray-900 text-green-300 rounded-xl p-4 text-sm overflow-x-auto leading-relaxed'>UPDATE users
SET username      = \'[deleted_\' || id || \']\',
    email         = \'deleted_\' || id || \'@earde.local\',
    password_hash = \'\'
WHERE id = $1</pre>
          <p class='mt-3 mb-3'>Your email is overwritten with an inert placeholder. Your password hash is wiped. Your username becomes an anonymous tombstone like <code class='bg-gray-100 px-1 rounded text-sm'>[deleted_42]</code>. Your votes, community memberships, moderator roles, bans, and notifications are hard-deleted by <code class='bg-gray-100 px-1 rounded text-sm'>ON DELETE CASCADE</code> constraints. Your session is immediately invalidated.</p>
          <p class='mb-3'><strong>Your posts and comments are not deleted.</strong> They remain in the database attributed to the <code class='bg-gray-100 px-1 rounded text-sm'>[deleted_N]</code> tombstone. This is an intentional design choice: hard-deleting your user row would cascade and destroy every reply ever written to your comments, breaking discussion threads for everyone else. The tombstone cannot be linked back to you (your email and credentials are gone) satisfying the pseudonymisation threshold of GDPR Art. 4(5) and Recital 26.</p>
          <p class='mb-3 text-sm bg-amber-50 border border-amber-200 rounded-xl p-3 text-amber-800'>If you also want your posts and comments deleted, email me at <a href='mailto:dami@earde.com' class='underline font-medium'>dami@earde.com</a> and I will remove them manually.</p>
          <p>Before deleting, you can export your posts and comments as JSON from the <a href='/settings' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>Settings page</a>.</p>
        </section>

        <!-- Art. 15-21 GDPR -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>7. Your GDPR Rights</h2>
          <p class='mb-4 text-sm'>To exercise any right, email <a href='mailto:dami@earde.com' class='text-[#C94C4C] underline hover:text-[#A83A3A] font-medium'>dami@earde.com</a>. I will respond within 30 days (Art. 12(3)), at no charge (Art. 12(5)). Many rights are also exercisable directly in the product:</p>
          <ul class='space-y-3'>
            <li><span class='font-semibold text-gray-900'>Access (Art. 15)</span> &mdash; Request a copy of all data held about you. <span class='text-[#C94C4C]'>In-product: <a href='/export-data' class='underline hover:text-[#A83A3A]'>Export data (JSON)</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Rectification (Art. 16)</span> &mdash; Correct inaccurate data. <span class='text-[#C94C4C]'>In-product: <a href='/settings' class='underline hover:text-[#A83A3A]'>Settings &rarr; Edit profile</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Erasure (Art. 17)</span> &mdash; Delete your account and anonymise your data. <span class='text-[#C94C4C]'>In-product: <a href='/settings' class='underline hover:text-[#A83A3A]'>Settings &rarr; Delete account</a>.</span> Or email me to also remove post/comment content.</li>
            <li><span class='font-semibold text-gray-900'>Restriction (Art. 18)</span> &mdash; Request restriction of processing pending a dispute. Email me.</li>
            <li><span class='font-semibold text-gray-900'>Portability (Art. 20)</span> &mdash; Receive your data in a machine-readable format. <span class='text-[#C94C4C]'>In-product: <a href='/export-data' class='underline hover:text-[#A83A3A]'>Export data (JSON)</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Object (Art. 21)</span> &mdash; Object to processing based on legitimate interest. Email me.</li>
          </ul>
        </section>

        <!-- Art. 13(2)(d) GDPR: supervisory authority -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>8. Supervisory Authority (Art. 13(2)(d))</h2>
          <p>If you believe your rights under the GDPR have been violated, you have the right to lodge a complaint with a supervisory authority. As I am based in Italy, the lead authority is the <strong>Garante per la protezione dei dati personali</strong> (<a href='https://www.garanteprivacy.it' class='text-[#C94C4C] underline hover:text-[#A83A3A]' target='_blank' rel='noopener noreferrer'>garanteprivacy.it</a>). You may also contact the authority in your own EU member state of residence.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-[#E0D9CC]'>9. Changes to This Policy</h2>
          <p>If the code changes in a way that affects data handling, this document will be updated and the date at the top of the page will reflect it. The source code remains the authoritative reference at all times.</p>
        </section>

      </div>
    </div>"
  in
  Components.layout ?user ~request ~title:"Privacy Policy" content

(* About page: honest manifesto — who we are, why we built this, how we differ.
   Written in first-person to match the voice of a founder-built product, not a corp. *)
let about_page ?user request =
  let content = "
    <div class='max-w-prose mx-auto mt-10 mb-16 px-4'>

      <h1 class='text-3xl font-extrabold text-gray-900 mb-2'>About Earde</h1>
      <p class='text-sm text-gray-400 mb-10'>The european discussion platform &mdash; built from the alps, for everyone.</p>

      <div class='space-y-10 text-gray-700 leading-7'>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Why we built this</h2>
          <p class='mb-3'>The internet&rsquo;s largest communities are almost exclusively hosted in the US, run by US corporations, and subject to US data practices. We believe Europe needs its own digital public square.</p>
          <p class='mb-3'>Earde is an <a href='https://github.com/earde-social/earde' class='text-[#C94C4C] underline hover:text-[#A83A3A]' target='_blank' rel='noopener noreferrer'>open-source</a>, deeply european discussion platform. We built this to be a GDPR-compliant sanctuary: hosted entirely on european servers, free from data harvesting, and strictly moderated against hate speech. We want to prove that a modern social aggregator can be incredibly fast, privacy-respecting, and community-driven without treating its users as products.</p>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Who is building this?</h2>
          <p class='mb-3'>Earde isn&rsquo;t a product of a Silicon Valley incubator, it&rsquo;s being built from the italian alps.</p>
          <p class='mb-3'>I&rsquo;m Dami, the developer. I have a degree in philosophy and a deep interest in formal logic. I also love to write code.</p>
          <p class='mb-3'>The name <em>Earde</em> comes from cimbrian, an ancient germanic language still spoken in a few isolated villages here in the alps. It means &ldquo;earth&rdquo;. We chose it because we want to build something grounded, a solid foundation for real human connection, far removed from the hyper-commercialized &ldquo;clouds&rdquo; of modern big tech.</p>
          <p>I am not doing this alone. My friend Nico is the other half of the brain behind Earde. He doesn&rsquo;t write a single line of code, and that is exactly why he is essential. Nico acts as the product manager. He ensures the platform is built for actual human beings, focusing on user experience, community dynamics, and keeping my engineering decisions aligned with our core philosophy.</p>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>The Alternatives (and why we are different)</h2>
          <p class='mb-4'>We aren&rsquo;t the first to try building a community platform. Here is exactly where we stand compared to the rest of the landscape:</p>
          <ul class='list-disc list-outside pl-5 space-y-3'>
            <li><strong>How is it different from Reddit?</strong> Aside from being hosted in the EU, Earde is fully <a href='https://github.com/earde-social/earde' class='text-[#C94C4C] underline hover:text-[#A83A3A]' target='_blank' rel='noopener noreferrer'>open-source</a>. We retain zero unnecessary data, run as little JavaScript as possible, and we try to build our code for speed, not for tracking your every click.</li>
            <li><strong>How is it different from Lemmy or Mastodon?</strong> We are fully centralized. We believe the Fediverse is a fantastic technical experiment, but it is fundamentally hostile to the average user. Earde is designed to be frictionless: you sign up in three seconds and start reading. No instances, no delays, no confusing server rules.</li>
            <li><strong>How is it different from Discuit?</strong> We actively want to raise capital to hire a real team, invest in marketing, and genuinely compete on a global scale (read our Investment Strategy below to see how we plan to do this without ruining the site).</li>
            <li><strong>How is it different from Squabbles?</strong> Squabbles launched with a &ldquo;free speech absolutist&rdquo; approach that quickly spiraled into toxic community management. We take a firm stance: we will enforce the deletion of hate speech, racism, and harassment. Freedom of discussion requires a safe environment to discuss in.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Transparency &amp; Privacy Policy</h2>
          <p>We believe in radical data minimalism. We do not track your off-site activity, we do not sell your email, and we permanently delete what needs to be deleted. For the exact technical details of how your data flows through our EU servers, read our <a href='/privacy' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>Technical Privacy Policy here</a>.</p>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Core features</h2>
          <p class='mb-4'>We baked our philosophy directly into the code.</p>
          <ul class='list-disc list-outside pl-5 space-y-3'>
            <li><strong>Public moderation logs:</strong> every ban, deleted post, and rule change made by moderators is visible in a public log.</li>
            <li><strong>The <em>Council of Equals</em>:</strong> there is no single &ldquo;dictator&rdquo; for a community. The top tier of moderation is shared by a council of up to 3 top mods with equal permissions.</li>
            <li><strong>The anti-squatter rule (3-month expiry):</strong> if a top mod is inactive on Earde for 3 months, they automatically lose their status.</li>
            <li><strong>Optional downvote shadowing:</strong> for highly sensitive communities, founders can enable downvote shadowing to prevent dogpiling and toxicity, while keeping the platform democratic.</li>
            <li><strong>No infinite scrolling:</strong> by default, Earde uses pagination. When you reach the bottom, it stops. We want to prevent doomscrolling.</li>
            <li><strong>No gamification:</strong> no awards, no paid badges, no gimmicks designed to artificially drum up engagement.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Community guidelines</h2>
          <p class='mb-4'>Earde is a curated space.</p>
          <ul class='list-disc list-outside pl-5 space-y-2'>
            <li>No illegal content, CSAM, or extreme violence.</li>
            <li>No hate speech, racism, or targeted harassment.</li>
            <li>No spam, self-promotion bots, or malicious links.</li>
          </ul>
          <p class='mt-4'>Individual communities can enforce stricter rules, but these global rules are non-negotiable.</p>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Our investment strategy (The &ldquo;enshittification&rdquo; dilemma)</h2>
          <p class='mb-3'>Let&rsquo;s address the elephant in the room. Most tech startups raise massive venture capital, which forces them to chase infinite hyper-growth. This inevitably leads to the &ldquo;enshittification&rdquo; of the product: aggressive ads, dark patterns, and algorithm manipulation just to satisfy a $400M valuation.</p>
          <p class='mb-3'>However, building a genuine Reddit competitor requires serious money for servers, legal compliance, and marketing. We believe funding is not a black-and-white issue. There is a difference between raising $400M from predatory Silicon Valley VCs and raising $2M from EU-aligned funds, angel investors, or european tech grants. A smaller, sensible funding round could allow us to hire a dedicated team and reach break-even profitability gracefully, without ever needing to sell out our users to hit impossible revenue targets.</p>
          <p>Furthermore, given Earde&rsquo;s explicitly pro-european, privacy-first political stance, we are entirely open to philanthropic funding models. So if you are an EU-aligned philanthropist, give us money! :) Let&rsquo;s build the european internet together. Reach out: <a href='mailto:dami@earde.com' class='text-[#C94C4C] underline hover:text-[#A83A3A]'>dami@earde.com</a>.</p>
        </section>

        <section>
          <h2 class='text-xl font-bold text-gray-900 mb-4'>Future Roadmap</h2>
          <ul class='list-disc list-outside pl-5 space-y-3'>
            <li><strong>Free public API:</strong> we want to build a free, robust API so developers can build third-party apps and moderation bots.</li>
            <li><strong>Toggleable infinite scroll:</strong> while we believe pagination is healthier, we will add an account-level toggle for users who prefer continuous scrolling.</li>
            <li><strong>Saved content:</strong> a private bookmarking system so you can save insightful posts and comments to read later, without relying on browser bookmarks.</li>
            <li><strong>Private messaging:</strong> we will implement user-to-user direct messages, but with a strict anti-abuse philosophy. It will be an opt-in system where you completely control who can reach your inbox, preventing the spam and harassment common on other platforms.</li>
            <li><strong>Modmail &amp; council tools:</strong> to make the Council of Equals work smoothly, we want to build internal communication tools for moderators to coordinate, and a transparent &ldquo;modmail&rdquo; system for users to appeal decisions or contact the moderation team securely.</li>
            <li><strong>Custom feeds:</strong> since we refuse to use algorithms to guess what you want to read, we will build a feature allowing you to group specific communities together (e.g., a custom &ldquo;tech &amp; science&rdquo; feed) for pure, chronological reading.</li>
          </ul>
          <p class='mt-4 text-sm text-gray-500'>While we have our baseline goals, Earde belongs to its users. The features we prioritize and build next will ultimately be the ones most requested by you. We build what the community actually asks for. So if you have any idea or feature request, please text me at dami@earde.com!
</p>
        
        </section>

      </div>
    </div>"
  in
  Components.layout ?user ~request ~title:"About" content

(* === MESSAGE PAGE === *)

(* Single shell for errors, successes, and info — avoids per-handler inline HTML
   fragments that diverge in style and don't inherit the shared layout/nav. *)
let msg_page ?user ~title ~message ~alert_type ~return_url request =
  let icon_html = match alert_type with
    | "success" ->
        "<div class='w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-6'><svg class='w-8 h-8 text-green-600' fill='none' stroke='currentColor' viewBox='0 0 24 24'><path stroke-linecap='round' stroke-linejoin='round' stroke-width='2.5' d='M5 13l4 4L19 7'/></svg></div>"
    | "info" ->
        "<div class='w-16 h-16 bg-blue-100 rounded-full flex items-center justify-center mx-auto mb-6'><svg class='w-8 h-8 text-blue-600' fill='none' stroke='currentColor' viewBox='0 0 24 24'><path stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z'/></svg></div>"
    | _ ->
        "<div class='w-16 h-16 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-6'><svg class='w-8 h-8 text-red-600' fill='none' stroke='currentColor' viewBox='0 0 24 24'><path stroke-linecap='round' stroke-linejoin='round' stroke-width='2.5' d='M6 18L18 6M6 6l12 12'/></svg></div>"
  in
  let content = Printf.sprintf "
    <div class='min-h-[70vh] flex items-center justify-center p-4'>
      <div class='bg-white border border-[#E0D9CC] rounded-2xl shadow-sm p-8 max-w-md w-full text-center'>
        %s
        <h1 class='text-2xl font-bold text-gray-900 mb-2'>%s</h1>
        <p class='text-gray-500 mb-6'>%s</p>
        <a href='%s' class='inline-block bg-[#C94C4C] text-white rounded-xl px-6 py-2.5 font-medium hover:bg-[#A83A3A] transition-colors'>Go Back</a>
      </div>
    </div>"
    icon_html (Components.html_escape title) (Components.html_escape message) return_url
  in
  Components.layout ?user ~request ~title content

(* === ADMIN === *)

(* admin_dashboard_page uses Components.layout so it inherits the navbar and the
   🛡️ Admin link — consistent shell for all admin-facing pages except hq_dashboard. *)
let admin_dashboard_page ?user ~(banned_users : user list) request =
  let csrf_token = Dream.csrf_tag request in
  let banned_rows =
    if banned_users = [] then
      "<p class='text-gray-500 italic text-sm'>No users are currently globally banned.</p>"
    else
      let rows = String.concat "\n" (List.map (fun (u : user) ->
        Printf.sprintf "
          <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
            <div>
              <a href='/u/%s' class='font-medium text-gray-900 hover:text-[#C94C4C] hover:underline'>u/%s</a>
              <span class='text-xs text-gray-400 ml-2'>%s</span>
            </div>
            <form action='/admin/unban/user/%d' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Lift global ban on u/%s?')\">
              %s
              <button type='submit' class='text-xs text-green-600 hover:text-green-800 font-bold border border-green-200 px-3 py-1 rounded transition'>✅ Unban</button>
            </form>
          </li>"
          u.username u.username u.email u.id u.username csrf_token
      ) banned_users) in
      Printf.sprintf "<ul class='divide-y divide-gray-100'>%s</ul>" rows
  in
  let content = Printf.sprintf "
    <div class='max-w-3xl mx-auto mt-8'>
      <div class='flex items-center justify-between mb-6'>
        <h1 class='text-3xl font-extrabold text-gray-900'>🛡️ Admin Dashboard</h1>
        <a href='/earde-hq-dashboard' class='text-sm text-[#C94C4C] hover:underline font-bold'>📊 KPI Dashboard &rarr;</a>
      </div>
      <div class='bg-white rounded-xl shadow-[0_2px_8px_rgba(60,54,48,0.06)] border border-[#E0D9CC] p-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-1'>Globally Banned Users</h2>
        <p class='text-xs text-gray-500 mb-4'>These accounts are blocked from logging in and posting anywhere on Earde.</p>
        %s
      </div>
    </div>" banned_rows
  in
  Components.layout ?user ~request ~title:"Admin Dashboard" content

(* === MODERATION LOG === *)

let mod_log_page ?user ~(community : Db.community) (actions : Db.mod_action list) request =
  let render_action (a : Db.mod_action) =
    let target_html = match a.target_id with
      | None -> ""
      | Some tid -> Printf.sprintf " &middot; target #%d" tid
    in
    Printf.sprintf "
    <tr class='border-b border-gray-100 hover:bg-gray-50'>
        <td class='py-3 px-4 text-sm text-gray-500 whitespace-nowrap'>%s</td>
        <td class='py-3 px-4 text-sm font-medium text-gray-900'><a href='/u/%s' class='text-[#C94C4C] hover:underline'>%s</a></td>
        <td class='py-3 px-4 text-sm'><span class='inline-block px-2 py-0.5 rounded bg-gray-100 text-gray-700 font-mono text-xs'>%s</span>%s</td>
        <td class='py-3 px-4 text-sm text-gray-600'>%s</td>
    </tr>"
      (Components.time_ago a.created_at)
      (Components.html_escape a.moderator_username) (Components.html_escape a.moderator_username)
      (Components.html_escape a.action_type) target_html
      (Components.html_escape a.reason)
  in
  let table_body =
    if actions = [] then
      "<tr><td colspan='4' class='py-10 text-center text-gray-400 italic'>No moderation actions recorded yet.</td></tr>"
    else String.concat "\n" (List.map render_action actions)
  in
  let content = Printf.sprintf "
    <div class='max-w-4xl mx-auto'>
        <div class='mb-6'>
            <a href='/c/%s' class='text-sm text-[#C94C4C] hover:underline'>&larr; Back to %s</a>
            <h1 class='text-2xl font-bold text-gray-900 mt-2'>Moderation Log</h1>
            <p class='text-sm text-gray-500 mt-1'>Public record of moderator actions in this community.</p>
        </div>
        <div class='bg-white border border-[#E0D9CC] rounded-xl shadow-sm overflow-hidden'>
            <table class='w-full'>
                <thead class='bg-gray-50 border-b border-[#E0D9CC]'>
                    <tr>
                        <th class='py-3 px-4 text-left text-xs font-semibold text-gray-500 uppercase tracking-wide'>When</th>
                        <th class='py-3 px-4 text-left text-xs font-semibold text-gray-500 uppercase tracking-wide'>Moderator</th>
                        <th class='py-3 px-4 text-left text-xs font-semibold text-gray-500 uppercase tracking-wide'>Action</th>
                        <th class='py-3 px-4 text-left text-xs font-semibold text-gray-500 uppercase tracking-wide'>Reason</th>
                    </tr>
                </thead>
                <tbody>%s</tbody>
            </table>
        </div>
    </div>"
    (Components.html_escape community.slug) (Components.html_escape community.name)
    table_body
  in
  Components.layout ?user ~request ~title:(community.name ^ " — Mod Log") content

(* Standalone HTML — intentionally outside Components.layout to prevent nav/JS
   assets from loading on an admin-only internal page that needs no public shell. *)
let hq_dashboard_page ((views, unique_visitors, signups), (content, active)) ~start_date ~end_date =
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
                <div class='text-3xl font-bold text-white'>%d</div>
                <div class='text-xs text-green-700'>ACTIVE CONTRIBUTORS</div>
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
                <h2 class='text-green-500 text-sm font-bold mb-4 tracking-widest'>ACTIVE CONTRIBUTORS</h2>
                <div class='text-5xl font-bold text-white mb-2'>%d</div>
                <div class='text-xs text-green-700'>ACTIVE USERS IN RANGE</div>
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
  start_date end_date active
  start_date end_date
  views unique_visitors
  signups
  content
  active
