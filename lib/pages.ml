open Db

(* === CORE FEED === *)

let index ?user user_votes current_page sort_mode ~admin_usernames ~moderated_communities (posts : post list) (user_communities : community list) request =
  let has_next = List.length posts = 20 in

  let posts_html =
    if posts = [] then
      "<div class='text-center py-10 text-gray-500 border border-dashed border-gray-200 rounded-lg'>It's quiet here. Too quiet. <br><a href='/new-community' class='text-[#0D9488] underline'>Create a community</a> and start posting!</div>"
    else String.concat "\n" (List.map (Components.render_post ~admin_usernames request user_votes) posts)
  in

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='/?sort=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50 transition'>&larr; Prev</a>" sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='/?sort=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50 transition'>Next &rarr;</a>" sort_mode (current_page + 1) in

  let get_sort_class s = if s = sort_mode then "text-[#0D9488] border-b-2 border-[#0D9488] pb-1" else "text-gray-500 hover:text-gray-800 transition" in
  let sort_menu = Printf.sprintf "
    <div class='flex space-x-6 mb-6 px-2 border-b border-gray-200'>
        <a href='/?sort=hot' class='font-bold text-sm tracking-wide uppercase %s'>🔥 Hot</a>
        <a href='/?sort=new' class='font-bold text-sm tracking-wide uppercase %s'>✨ New</a>
        <a href='/?sort=top' class='font-bold text-sm tracking-wide uppercase %s'>🏆 Top</a>
    </div>" (get_sort_class "hot") (get_sort_class "new") (get_sort_class "top")
  in

  let sidebar_html = Components.left_sidebar ?user ~moderated_communities user_communities in

  let content = Printf.sprintf "
    <div class='flex flex-col lg:flex-row gap-6'>
        <div class='w-full lg:w-1/4 hidden lg:block'><div class='sticky top-20'>%s</div></div>
        <div class='w-full lg:w-2/4 min-w-0'>
            <div class='flex justify-between items-center mb-4'>
                <h1 class='text-2xl font-bold text-gray-900'>Global Feed</h1>
            </div>
            <div class='block lg:hidden mb-6 bg-blue-50 border border-blue-100 rounded-lg p-4 shadow-sm'><h3 class='text-sm font-bold text-blue-900 mb-1'>Talk to me!</h3><p class='text-xs text-blue-800 mb-3 leading-relaxed'>For feature requests, ideas, critiques, if you are a Reddit mod and want to become a mod on the specular community here, or just to say hi!</p><a href='https://t.me/tolwiz' target='_blank' rel='noopener noreferrer' class='w-full flex items-center justify-center gap-2 bg-blue-600 hover:bg-blue-700 text-white text-sm font-semibold py-2 rounded-md transition-colors'>&#128172; Text me (the dev)!</a></div>
            %s <div>%s</div>
            <div class='flex justify-between items-center mt-8 mb-4'>
                <div>%s</div><div class='text-sm text-gray-500 font-bold'>Page %d</div><div>%s</div>
            </div>
        </div>
        <div class='w-full lg:w-1/4'><div class='bg-white p-5 rounded-lg border border-gray-200 sticky top-20'><h2 class='text-sm font-semibold text-gray-800 mb-1'>Earde</h2><p class='text-xs text-gray-500 mb-4'>Your personal frontpage.</p><div class='flex flex-col space-y-2'><a href='/new-post' class='w-full bg-[#0D9488] text-white text-center py-2 rounded-md font-semibold text-sm hover:bg-teal-700 transition'>Create Post</a><a href='/new-community' class='w-full bg-white text-[#0D9488] border border-[#0D9488] text-center py-2 rounded-md font-semibold text-sm hover:bg-teal-50 transition'>Create Community</a></div></div><div class='mt-6 bg-blue-50 border border-blue-100 rounded-lg p-4 shadow-sm'><h3 class='text-sm font-bold text-blue-900 mb-1'>Talk to me!</h3><p class='text-xs text-blue-800 mb-3 leading-relaxed'>For feature requests, ideas, critiques, if you are a Reddit mod and want to become a mod on the specular community here, or just to say hi!</p><a href='https://t.me/tolwiz' target='_blank' rel='noopener noreferrer' class='w-full flex items-center justify-center gap-2 bg-blue-600 hover:bg-blue-700 text-white text-sm font-semibold py-2 rounded-md transition-colors'>&#128172; Text me (the dev)!</a></div></div>
    </div>"
    sidebar_html sort_menu posts_html prev_btn current_page next_btn
  in
  Components.layout ?user ~request ~title:"Home" content

(* === AUTHENTICATION === *)

let signup_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-lg shadow-md mt-10'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800 text-center'>Join Earde</h1>

        <form action='/signup' method='POST' class='space-y-4'>
            %s

            <div>
                <label class='block text-sm font-medium text-gray-700'>Username</label>
                <input type='text' name='username' required
                       class='mt-1 block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='Choose a unique username'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700'>Email Address</label>
                <input type='email' name='email' required
                       class='mt-1 block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='you@example.com'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700'>Password</label>
                <input type='password' name='password' required
                       class='mt-1 block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='Minimum 8 characters'>
            </div>

            <div class='flex items-start mt-4 mb-6'>
                <div class='flex items-center h-5'>
                    <input id='privacy' name='privacy' type='checkbox' required
                           class='w-4 h-4 border border-gray-300 rounded bg-gray-50 focus:ring-2 focus:ring-[#0D9488] cursor-pointer'>
                </div>
                <label for='privacy' class='ml-2 text-sm font-medium text-gray-700 cursor-pointer'>
                    I agree to the <a href='/privacy' target='_blank' class='text-[#0D9488] hover:underline font-bold'>Privacy Policy</a> and consent to data processing.
                </label>
            </div>

            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded-md hover:bg-teal-700 font-semibold transition'>
                Sign Up
            </button>
        </form>

        <div class='mt-6 text-center text-sm'>
            <p class='text-gray-600'>Already have an account?</p>
            <a href='/login' class='font-medium text-[#0D9488] hover:text-[#0D9488]'>Log in</a>
        </div>
    </div>"
    csrf_token
  in
  Components.layout ?user ~request ~title:"Sign Up" content

let login_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-lg shadow-sm border border-gray-200 mt-10'>
        <h1 class='text-2xl font-bold mb-6 text-gray-900 text-center'>Welcome back</h1>
        <form action='/login' method='POST' class='space-y-4'>
            %s
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Username or Email</label>
                <input type='text' name='identifier' required class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border' placeholder='tolwiz or tolwiz@example.com'>
            </div>
            <div>
                <div class='flex justify-between items-center mb-1'>
                    <label class='block text-sm font-medium text-gray-700'>Password</label>
                    <a href='/forgot-password' class='text-xs text-[#0D9488] hover:text-indigo-800 hover:underline tabindex='-1'>Forgot password?</a>
                </div>
                <input type='password' name='password' required class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
            </div>
            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded font-bold hover:bg-teal-700 transition shadow-sm'>Log In</button>
        </form>
        <div class='mt-6 text-center text-sm text-gray-600'>
            Don't have an account? <a href='/signup' class='text-[#0D9488] font-bold hover:underline'>Sign up</a>
        </div>
    </div>" csrf_token
  in
  Components.layout ~title:"Log In" ?user ~request content

let forgot_password_page request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto bg-white p-8 rounded-lg shadow-sm border border-gray-200 mt-10'>
        <h1 class='text-2xl font-bold mb-2 text-gray-900 text-center'>Forgot Password?</h1>
        <p class='text-gray-600 text-sm text-center mb-6'>Enter your email address and we'll send you a reset link.</p>
        <form action='/forgot-password' method='POST' class='space-y-4'>
            %s
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Email address</label>
                <input type='email' name='email' required class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border' placeholder='you@example.com'>
            </div>
            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded font-bold hover:bg-teal-700 transition shadow-sm'>Send Reset Link</button>
        </form>
        <div class='mt-4 text-center text-sm text-gray-500'>
            <a href='/login' class='text-[#0D9488] hover:underline'>Back to login</a>
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
    <div class='max-w-md mx-auto bg-white p-8 rounded-lg shadow-sm border border-gray-200 mt-10'>
        <h1 class='text-2xl font-bold mb-2 text-gray-900 text-center'>Set New Password</h1>
        <p class='text-gray-600 text-sm text-center mb-6'>Enter a new password for your account.</p>
        %s
        <form action='/reset-password' method='POST' class='space-y-4'>
            %s
            <input type='hidden' name='token' value='%s'>
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>New password</label>
                <input type='password' name='password' required minlength='8' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
            </div>
            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Confirm new password</label>
                <input type='password' name='confirm_password' required minlength='8' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
            </div>
            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded font-bold hover:bg-teal-700 transition shadow-sm'>Reset Password</button>
        </form>
    </div>" error_html csrf_token token
  in Components.layout ~noindex:true ~request ~title:"Reset Password" content

(* === COMMUNITY === *)

let new_community_form ?user request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-lg shadow-md border border-gray-200'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800'>Create a New Community</h1>

        <form action='/communities' method='POST' class='space-y-6'>
            %s

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Community Name</label>
                <input type='text' name='name' required
                       class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='e.g., Italian Cuisine'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>URL Slug</label>
                <div class='flex rounded-md shadow-sm'>
                    <span class='inline-flex items-center px-3 rounded-l-md border border-r-0 border-gray-300 bg-gray-50 text-gray-500 text-sm'>
                        /c/
                    </span>
                    <input type='text' name='slug' required
                           class='flex-1 block w-full rounded-none rounded-r-md border-gray-300 focus:border-[#0D9488] focus:ring-[#0D9488] p-2 border'
                           placeholder='italian-cuisine'>
                </div>
                <p class='mt-1 text-xs text-gray-500'>Lowercase, no spaces.</p>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Description</label>
                <textarea name='description' rows='3'
                          class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'></textarea>
            </div>

            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded-md hover:bg-teal-700 font-semibold transition'>
                Create Community
            </button>
        </form>
    </div>"
  csrf_token
  in
  Components.layout ?user ~request ~title:"New Community" content

let community_page ?user ~is_member ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames ~user_communities ~moderated_communities user_votes current_page sort_mode (community : community) (posts : post list) request =
  let csrf_token = Dream.csrf_tag request in
  let is_admin = Dream.session_field request "is_admin" = Some "true" in
  let has_next = List.length posts = 20 in

  let posts_html =
    if posts = [] then "<div class='bg-gray-50 p-12 text-center rounded-lg border border-dashed border-gray-300 text-gray-500'>No posts yet. Be the first to share something!</div>"
    else String.concat "\n" (List.map (Components.render_post ~is_current_user_mod ~mod_usernames ~admin_usernames ~banned_usernames request user_votes) posts)
  in

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='/c/%s?sort=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50 transition'>&larr; Previous</a>" community.slug sort_mode (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='/c/%s?sort=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50 transition'>Next &rarr;</a>" community.slug sort_mode (current_page + 1) in

  let get_sort_class s = if s = sort_mode then "text-[#0D9488] border-b-2 border-[#0D9488] pb-1" else "text-gray-500 hover:text-gray-800 transition" in
  let sort_menu = Printf.sprintf "
    <div class='flex space-x-6 mb-6 px-2 border-b border-gray-200 mt-6'>
        <a href='/c/%s?sort=hot' class='font-bold text-sm tracking-wide uppercase %s'>🔥 Hot</a>
        <a href='/c/%s?sort=new' class='font-bold text-sm tracking-wide uppercase %s'>✨ New</a>
        <a href='/c/%s?sort=top' class='font-bold text-sm tracking-wide uppercase %s'>🏆 Top</a>
    </div>" community.slug (get_sort_class "hot") community.slug (get_sort_class "new") community.slug (get_sort_class "top")
  in

  let membership_btn =
    match user with
    | None -> ""
    | Some _ ->
        if is_member then Printf.sprintf "<form action='/leave' method='POST' class='m-0 p-0'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='rounded-full px-5 py-1.5 border border-gray-300 hover:bg-gray-50 text-sm font-medium text-gray-700 transition'>Leave</button></form>" csrf_token community.id community.slug
        else Printf.sprintf "<form action='/join' method='POST' class='m-0 p-0'>%s<input type='hidden' name='community_id' value='%d'><input type='hidden' name='redirect_to' value='/c/%s'><button type='submit' class='rounded-full px-5 py-1.5 bg-[#0D9488] text-white text-sm font-semibold hover:bg-teal-700 transition'>Join</button></form>" csrf_token community.id community.slug
  in

  (* Admins see the edit button without needing mod status — global authority. *)
  let settings_btn =
    if is_current_user_mod || is_admin then
      Printf.sprintf "<a href='/c/%s/settings' class='rounded-full px-4 py-1.5 border border-gray-300 hover:bg-gray-50 text-sm font-medium text-gray-700 transition'>Edit Community</a>" community.slug
    else ""
  in

  (* Create Post shortcut lives in the header, not the sidebar, for immediate access. *)
  let create_post_btn =
    match user with
    | None -> ""
    | Some _ ->
        Printf.sprintf "<a href='/new-post?community=%s' class='rounded-full px-5 py-1.5 bg-[#0D9488] text-white text-sm font-semibold hover:bg-teal-700 transition'>+ Post</a>" community.slug
  in

  let banner_html =
    match community.banner_url with
    | Some url when url <> "" ->
        Printf.sprintf "<div class='h-24 md:h-40 w-full bg-gray-100 rounded-2xl overflow-hidden'><img src='%s' class='w-full h-full object-cover' alt='banner'></div>" url
    | _ ->
        "<div class='h-24 md:h-40 w-full bg-gradient-to-r from-teal-50 to-gray-100 rounded-2xl'></div>"
  in

  let avatar_html =
    match community.avatar_url with
    | Some url when url <> "" ->
        Printf.sprintf "<img src='%s' class='w-20 h-20 md:w-24 md:h-24 rounded-full border-4 border-white bg-white shadow-sm object-cover'>" url
    | _ ->
        Printf.sprintf "<div class='w-20 h-20 md:w-24 md:h-24 rounded-full border-4 border-white bg-[#0D9488] flex items-center justify-center text-white text-2xl font-bold shadow-sm'>%s</div>"
          (String.uppercase_ascii (String.sub community.name 0 1))
  in

  let mods_sidebar_html =
    if mod_usernames = [] then "<p class='text-xs text-gray-400 italic'>No moderators yet.</p>"
    else
      let links = String.concat "\n" (List.map (fun u ->
        Printf.sprintf "<li><a href='/u/%s' class='text-sm text-gray-700 hover:text-[#0D9488] transition'>u/%s</a></li>" (Components.html_escape u) (Components.html_escape u)
      ) mod_usernames) in
      Printf.sprintf "<ul class='space-y-1'>%s</ul>" links
  in

  (* Three separate sidebar cards: clearer hierarchy than one combined card. *)
  let community_info_card = Printf.sprintf "
    <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-5'>
        <h2 class='font-bold text-gray-900 mb-1'>%s</h2>
        <div class='text-xs text-[#0D9488] font-mono mb-3'>/c/%s</div>
        <p class='text-sm text-gray-600'>%s</p>
    </div>"
    (Components.html_escape community.name) (Components.html_escape community.slug)
    (Components.html_escape (Option.value ~default:"No description." community.description))
  in

  let rules_card =
    match community.rules with
    | Some rules when rules <> "" ->
        Printf.sprintf "
    <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-5'>
        <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-3'>Rules</h3>
        <p class='text-xs text-gray-600 whitespace-pre-wrap'>%s</p>
    </div>" (Components.html_escape rules)
    | _ -> ""
  in

  let mods_card = Printf.sprintf "
    <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-5'>
        <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-3'>Moderators</h3>
        %s
    </div>"
    mods_sidebar_html
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
    community_info_card rules_card mods_card
  in
  Components.layout ?user ~request ~title:community.name content

let community_settings_page ?user ~(community : community) ~(mods : user list) ~(banned_users : user list) request =
  let csrf_token = Dream.csrf_tag request in
  let mod_count = List.length mods in

  let mod_rows = String.concat "\n" (List.map (fun (m : user) ->
    let remove_btn =
      (* At least one mod must remain — enforce at render time and again server-side
         to close the race window between page load and form submission. *)
      if mod_count > 1 then
        Printf.sprintf "
          <form action='/remove-mod' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Remove this moderator? This action cannot be undone.')\">
            %s
            <input type='hidden' name='target_user_id' value='%d'>
            <input type='hidden' name='community_id' value='%d'>
            <input type='hidden' name='community_slug' value='%s'>
            <button type='submit' class='text-xs text-red-500 hover:text-red-700 font-bold border border-red-200 px-2 py-1 rounded transition'>Remove</button>
          </form>"
          csrf_token m.id community.id (Components.html_escape community.slug)
      else
        "<span class='text-xs text-gray-400 italic'>Last mod</span>"
    in
    Printf.sprintf "
      <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
        <a href='/u/%s' class='font-medium text-gray-800 hover:text-[#0D9488] hover:underline'>u/%s</a>
        %s
      </li>"
      (Components.html_escape m.username) (Components.html_escape m.username) remove_btn
  ) mods) in

  let banned_section =
    if banned_users = [] then
      "<p class='text-gray-500 italic text-sm'>No users are currently banned from this community.</p>"
    else
      let rows = String.concat "\n" (List.map (fun (b : user) ->
        Printf.sprintf "
          <li class='flex items-center justify-between py-3 border-b border-gray-100 last:border-0'>
            <a href='/u/%s' class='font-medium text-gray-800 hover:text-[#0D9488] hover:underline'>u/%s</a>
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
        <a href='/c/%s' class='text-sm text-[#0D9488] hover:underline'>&larr; Back to Community</a>
      </div>

      <div class='bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Edit Community</h2>
        <form action='/update-community' method='POST' class='space-y-4'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='hidden' name='community_slug' value='%s'>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Description</label>
            <textarea name='description' rows='3'
                      class='w-full rounded-md border border-gray-300 p-2 text-sm focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none'>%s</textarea>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Community Rules</label>
            <textarea name='rules' rows='5'
                      class='w-full rounded-md border border-gray-300 p-2 text-sm focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none'>%s</textarea>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Avatar URL</label>
            <input type='url' name='avatar_url' value='%s'
                   placeholder='https://example.com/avatar.png'
                   class='w-full rounded-md border border-gray-300 p-2 text-sm focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none'>
          </div>
          <div>
            <label class='block text-sm font-medium text-gray-700 mb-1'>Banner URL</label>
            <input type='url' name='banner_url' value='%s'
                   placeholder='https://example.com/banner.png'
                   class='w-full rounded-md border border-gray-300 p-2 text-sm focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none'>
          </div>
          <button type='submit' class='bg-[#0D9488] text-white px-4 py-2 rounded-md font-semibold text-sm hover:bg-teal-700 transition'>
            Save Changes
          </button>
        </form>
      </div>

      <div class='bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Moderators</h2>
        <ul>%s</ul>
      </div>

      <div class='bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Add Moderator</h2>
        <form action='/add-mod' method='POST' class='flex items-center space-x-3'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='hidden' name='community_slug' value='%s'>
          <input type='text' name='username' required placeholder='Username to promote'
                 class='flex-1 rounded-md border-gray-300 shadow-sm focus:border-[#0D9488] p-2 border text-sm'>
          <button type='submit' class='bg-[#0D9488] text-white px-4 py-2 rounded font-bold text-sm hover:bg-teal-700 transition shadow-sm'>
            Add Mod
          </button>
        </form>
      </div>

      <div class='bg-white rounded-lg shadow-sm border border-gray-200 p-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-4'>Banned Users</h2>
        %s
        <form action='/ban-community-user' method='POST' class='flex items-center space-x-3 mt-4'>
          %s
          <input type='hidden' name='community_id' value='%d'>
          <input type='text' name='target_username' required placeholder='Username to ban'
                 class='flex-1 rounded-md border-gray-300 shadow-sm focus:border-[#0D9488] p-2 border text-sm'>
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
    mod_rows
    csrf_token community.id community.slug
    banned_section csrf_token community.id
  in
  Components.layout ?user ~request ~title:(Printf.sprintf "Settings — /c/%s" community.slug) content

(* === POST === *)

let choose_community_page ?user (communities : community list) =
  let render_option (community : community) =
    Printf.sprintf "
    <a href='/new-post?community=%s' class='block bg-white p-4 rounded-lg border border-gray-200 hover:border-[#0D9488] hover:shadow-md transition flex justify-between items-center group'>
        <div>
            <span class='font-bold text-lg text-gray-800 group-hover:text-[#0D9488]'>%s</span>
            <span class='text-sm text-gray-500 ml-2'>/c/%s</span>
        </div>
        <div class='bg-gray-100 text-gray-600 px-3 py-1 rounded text-sm group-hover:bg-[#0D9488] group-hover:text-white transition'>
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
            <a href='/new-community' class='text-[#0D9488] font-bold hover:underline'>Create a new Community</a>
        </div>
    </div>"
    list_html
  in
  Components.layout ?user ~title:"Choose Community" content

let join_to_post_page ?user (community : community) request =
  let csrf_token = Dream.csrf_tag request in
  let content = Printf.sprintf "
    <div class='max-w-md mx-auto mt-10 p-8 bg-white rounded-lg shadow-md border border-gray-200 text-center'>
        <div class='text-6xl mb-4'>🔒</div>
        <h1 class='text-2xl font-bold text-gray-800 mb-2'>Members Only</h1>
        <p class='text-gray-600 mb-6'>
            You must be a member of <span class='font-bold text-[#0D9488]'>/c/%s</span> to create a post here.
        </p>

        <form action='/join' method='POST'>
            %s
            <input type='hidden' name='community_id' value='%d'>
            <input type='hidden' name='redirect_to' value='/new-post?community=%s'>

            <button type='submit' class='w-full bg-[#0D9488] text-white font-bold py-3 px-4 rounded-lg hover:bg-teal-700 transition shadow-lg transform hover:-translate-y-0.5'>
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
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-lg shadow-md border border-gray-200'>
        <h1 class='text-2xl font-bold mb-6 text-gray-800'>Post to <span class='text-[#0D9488]'>/c/%s</span></h1>

        <form action='/posts' method='POST' class='space-y-6'>
            %s
            <input type='hidden' name='community_id' value='%d'>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Title</label>
                <input type='text' name='title' required
                       class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='An interesting title'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>URL (Optional)</label>
                <input type='url' name='url'
                       class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                       placeholder='https://example.com'>
            </div>

            <div>
                <label class='block text-sm font-medium text-gray-700 mb-1'>Text (Optional)</label>
                <textarea name='content' rows='6'
                          class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'
                          placeholder='Share your thoughts...'></textarea>
            </div>

            <button type='submit' class='w-full bg-[#0D9488] text-white py-2 px-4 rounded-md hover:bg-teal-700 font-semibold transition'>
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
                <textarea name='content' required rows='3' class='w-full p-3 border border-gray-200 rounded-lg shadow-sm focus:outline-none focus:ring-1 focus:ring-[#0D9488] focus:border-[#0D9488] text-sm' placeholder='Write a reply...'></textarea>
                <div class='flex justify-end gap-2 mt-2'>
                    <button type='button' onclick=\"document.getElementById('reply-form-%d').classList.toggle('hidden')\" class='text-sm text-gray-500 font-medium hover:text-gray-700 px-3 py-1.5'>Cancel</button>
                    <button type='submit' class='bg-[#0D9488] text-white text-sm font-medium px-4 py-1.5 rounded-full hover:bg-teal-700 transition'>Post Reply</button>
                </div>
            </form>" c.id csrf_token post.id c.id c.id
          else ""
        in

        let current_vote = Option.value ~default:0 (List.assoc_opt c.id user_comment_votes) in

        let up_color = if current_vote = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
        let down_color = if current_vote = -1 then "text-[#0D9488]" else "text-gray-400 hover:text-[#0D9488]" in

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
        let delete_comment_btn =
          if is_comment_deleted then ""
          else match current_user with
          | Some u when u = c.username || is_admin || (is_current_user_mod && not comment_target_is_admin) ->
              (* community_id is forwarded so delete_comment_handler can verify mod status
                 server-side without an extra DB round-trip to resolve comment → post → community. *)
              Printf.sprintf "<form action='/delete-comment' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Do you really want to delete this comment? This action cannot be undone.')\">
                  %s <input type='hidden' name='comment_id' value='%d'>
                  <input type='hidden' name='community_id' value='%d'>
                  <button type='submit' class='text-xs %s hover:text-red-700 font-bold'>🗑️</button>
              </form>" csrf_token c.id post.community_id (if u <> c.username then "text-red-600" else "text-red-500")
          | _ -> ""
        in

        (* Ban button mirrors render_post's ban_btn logic; closed over post.community_id.
           banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
           Admins share mod authority, so the button is granted to them as well. *)
        let ban_comment_btn =
          if (is_current_user_mod || is_admin) && not (comment_target_is_admin && not is_admin) then
            match current_user with
            | Some u when u <> c.username && not (Components.is_deleted_user c.username) ->
                if List.mem c.username banned_usernames then
                  "<span class='text-xs text-red-600 font-bold'>🚫 Banned</span>"
                else
                  Printf.sprintf "<form action='/ban-community-user' method='POST' class='inline m-0 p-0' onsubmit=\"confirmModal(event, 'Ban u/%s from this community? This action cannot be undone.')\">%s<input type='hidden' name='target_username' value='%s'><input type='hidden' name='community_id' value='%d'><button type='submit' class='text-xs text-orange-500 hover:text-orange-700 font-bold' title='Ban user from this community'>🔨</button></form>"
                    c.username csrf_token c.username post.community_id
            | _ -> ""
          else ""
        in

        let avatar_html = match c.avatar_url with
          | Some url when url <> "" -> Printf.sprintf "<img src='%s' alt='Avatar' class='w-5 h-5 rounded-full object-cover shadow-sm border border-gray-200'>" url
          | _ -> Printf.sprintf "<div class='w-5 h-5 bg-teal-100 rounded-full flex items-center justify-center text-[10px] text-[#0D9488] font-bold shadow-sm border border-teal-200'>%s</div>" (String.sub c.username 0 1 |> String.uppercase_ascii)
        in

        let upvote_html = match current_user with
          | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-xs font-bold leading-none'>▲</button></form>" csrf_token c.id up_action up_color
          | None -> "<a href='/login' class='text-gray-400 hover:text-orange-500 text-xs font-bold leading-none'>▲</a>"
        in
        let downvote_html = match current_user with
          | Some _ -> Printf.sprintf "<form action='/vote-comment' method='POST' class='m-0 p-0'>%s<input type='hidden' name='comment_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-xs font-bold leading-none'>▼</button></form>" csrf_token c.id down_action down_color
          | None -> "<a href='/login' class='text-gray-400 hover:text-[#0D9488] text-xs font-bold leading-none'>▼</a>"
        in

        let op_badge =
          if c.username = post.username then
            "<span class='ml-1.5 font-bold text-[10px] bg-teal-100 text-teal-700 px-1.5 py-0.5 rounded'>OP</span>"
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
            "<div id='comment-children-%d' class='pl-3 border-l-2 border-gray-200 ml-2.5 mt-2'>%s</div>"
            c.id nested_html
        in

        Printf.sprintf "
        <div class='mt-3 hover:bg-gray-50 transition rounded-r pr-2 py-1'>
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
                    <div class='flex items-center gap-1.5 bg-gray-50 border border-gray-200 rounded-full px-2 py-0.5'>
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
          <textarea name='content' required rows='2' class='w-full border border-gray-200 rounded-lg px-4 py-3 text-sm bg-gray-50 focus:bg-white focus:ring-1 focus:ring-[#0D9488] focus:border-[#0D9488] resize-y transition-colors placeholder-gray-400' placeholder='Add a comment...'></textarea>
          <div class='flex justify-end mt-2 mb-8'>
              <button type='submit' class='px-4 py-1.5 text-sm font-medium bg-[#0D9488] text-white rounded-full hover:bg-teal-700 transition-colors shadow-sm'>Comment</button>
          </div>
      </form>" csrf_token post.id
    else
      Printf.sprintf "
      <div class='mt-6 mb-8 p-6 bg-teal-50 rounded-lg border border-teal-100 text-center'>
          <h3 class='text-gray-900 font-bold mb-2'>Join the discussion</h3>
          <p class='text-teal-700 text-sm mb-4'>You must be a member of /c/%s to comment.</p>
          <form action='/join' method='POST'>
              %s
              <input type='hidden' name='community_id' value='%d'>
              <input type='hidden' name='redirect_to' value='/p/%d'>
              <button type='submit' class='bg-[#0D9488] text-white px-6 py-2 rounded-full font-bold hover:bg-teal-700 transition shadow-sm'>Join /c/%s</button>
          </form>
      </div>" post.community_slug csrf_token post.community_id post.id post.community_slug
  in

  let post_content = match post.content with | Some c -> Printf.sprintf "<div class='text-sm text-gray-800 leading-relaxed whitespace-pre-wrap break-words mt-2 mb-3'>%s</div>" (Components.html_escape c) | None -> "" in
  let link_content = match post.url with | Some u -> Printf.sprintf "<div class='mb-6'><a href='%s' target='_blank' class='text-blue-600 hover:underline break-all'>🔗 %s</a></div>" (Components.safe_url u) (Components.html_escape u) | None -> "" in

  let current_vote_direction = match List.assoc_opt post.id user_post_votes with Some d -> d | None -> 0 in
  let up_color = if current_vote_direction = 1 then "text-orange-500" else "text-gray-400 hover:text-orange-500" in
  let down_color = if current_vote_direction = -1 then "text-[#0D9488]" else "text-gray-400 hover:text-[#0D9488]" in
  let up_action = if current_vote_direction = 1 then 0 else 1 in
  let down_action = if current_vote_direction = -1 then 0 else -1 in
  (* Voting pill mirrors the pattern in components.ml render_post: toggle by sending
     direction=0 when already voted, avoiding a separate undo endpoint.
     Comments and Share pills share the same action bar to keep post metadata
     actions cohesive and avoid redundant top-of-post share button placement. *)
  let comments_pill =
    Printf.sprintf "<div class='flex items-center gap-1.5 bg-gray-50 border border-gray-200 rounded-full px-3 py-1.5 text-sm font-medium text-gray-700'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8 12h.01M12 12h.01M16 12h.01M21 12c0 4.418-4.03 8-9 8a9.863 9.863 0 01-4.255-.949L3 20l1.395-3.72C3.512 15.042 3 13.574 3 12c0-4.418 4.03-8 9-8s9 3.582 9 8z'/></svg>%d</div>"
      post.comment_count
  in
  let share_pill =
    Printf.sprintf "<button type='button' onclick='copyPostLink(\"/p/%d\", this)' class='flex items-center gap-1.5 bg-gray-50 border border-gray-200 rounded-full px-3 py-1.5 text-sm font-medium text-gray-700 hover:bg-gray-100 transition-colors cursor-pointer'><svg xmlns='http://www.w3.org/2000/svg' class='w-4 h-4' fill='none' viewBox='0 0 24 24' stroke='currentColor' stroke-width='2'><path stroke-linecap='round' stroke-linejoin='round' d='M8.684 13.342C8.886 12.938 9 12.482 9 12c0-.482-.114-.938-.316-1.342m0 2.684a3 3 0 110-2.684m0 2.684l6.632 3.316m-6.632-6l6.632-3.316m0 0a3 3 0 105.367-2.684 3 3 0 00-5.367 2.684zm0 9.316a3 3 0 105.368 2.684 3 3 0 00-5.368-2.684z'/></svg>Share</button>"
      post.id
  in
  let voting_pill =
    match current_user with
    | Some _ ->
        Printf.sprintf "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-gray-200 rounded-full px-3 py-1.5'><form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-sm font-bold leading-none'>▲</button></form><span class='text-sm font-semibold text-gray-700'>%d</span><form action='/vote' method='POST' class='m-0 p-0 flex'>%s<input type='hidden' name='post_id' value='%d'><input type='hidden' name='direction' value='%d'><button type='submit' class='%s text-sm font-bold leading-none'>▼</button></form></div>%s%s</div>"
          csrf_token post.id up_action up_color post.score csrf_token post.id down_action down_color comments_pill share_pill
    | None ->
        Printf.sprintf "<div class='flex items-center gap-3 mt-2 mb-6'><div class='flex items-center gap-2 bg-gray-50 border border-gray-200 rounded-full px-3 py-1.5'><a href='/login' class='text-gray-400 hover:text-orange-500 text-sm font-bold leading-none'>▲</a><span class='text-sm font-semibold text-gray-700'>%d</span><a href='/login' class='text-gray-400 hover:text-[#0D9488] text-sm font-bold leading-none'>▼</a></div>%s%s</div>"
          post.score comments_pill share_pill
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
  let delete_post_btn =
    if is_post_deleted then ""
    else match current_user with
      | Some u when u = post.username || is_admin || (is_current_user_mod && not post_target_is_admin) ->
        Printf.sprintf "<form action='/delete-post' method='POST' class='inline m-0 p-0 ml-3' onsubmit=\"confirmModal(event, 'Do you really want to delete this post? This action cannot be undone.')\">
            %s <input type='hidden' name='post_id' value='%d'>
            <button type='submit' class='text-sm %s hover:text-red-700 font-bold'>🗑️ Delete</button>
        </form>" csrf_token post.id (if u <> post.username then "text-red-600" else "text-red-500")
    | _ -> ""
  in

  (* post_page renders the post inline, not via Components.render_post, so ban_post_btn
     must be computed here separately — same guard logic as components.ml's ban_btn.
     banned_usernames replaces the hammer with a badge — prevents double-ban confusion.
     Admins share mod authority, so the button is granted to them as well. *)
  let ban_post_btn =
    if (is_current_user_mod || is_admin) && not (post_target_is_admin && not is_admin) then
      match current_user with
      | Some u when u <> post.username
          && not (String.length post.username >= 9 && String.sub post.username 0 9 = "[deleted_") ->
          if List.mem post.username banned_usernames then
            "<span class='text-sm text-red-600 font-bold ml-1'>🚫 Banned</span>"
          else
            Printf.sprintf "<form action='/ban-community-user' method='POST' class='inline m-0 p-0 ml-1' onsubmit=\"confirmModal(event, 'Ban this user from this community? This action cannot be undone.')\">%s<input type='hidden' name='target_username' value='%s'><input type='hidden' name='community_id' value='%d'><button type='submit' class='text-sm text-orange-500 hover:text-orange-700 font-bold' title='Ban user from this community'>🔨 Ban</button></form>"
              csrf_token (Components.html_escape post.username) post.community_id
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
        Printf.sprintf "<li><a href='/u/%s' class='text-sm text-gray-700 hover:text-[#0D9488] transition'>u/%s</a></li>" (Components.html_escape u) (Components.html_escape u)
      ) mod_usernames) in
      Printf.sprintf "<ul class='space-y-1'>%s</ul>" links
  in

  let post_right_sidebar = Printf.sprintf "
    <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-5'>
        <h2 class='font-bold text-gray-900 mb-1'>%s</h2>
        <div class='text-xs text-[#0D9488] font-mono mb-3'>/c/%s</div>
        <p class='text-sm text-gray-600'>%s</p>
        %s
        <a href='/new-post?community=%s' class='bg-[#0D9488] text-white rounded-lg px-4 py-2 w-full block text-center mt-4 hover:bg-teal-700 transition text-sm font-semibold'>+ Create Post</a>
        <div class='mt-4 pt-4 border-t border-gray-100'>
            <h3 class='text-xs font-bold text-gray-500 uppercase tracking-wider mb-2'>Moderators</h3>
            %s
        </div>
    </div>"
    (Components.html_escape community.name) (Components.html_escape community.slug)
    (Components.html_escape (Option.value ~default:"No description." community.description))
    post_rules_html (Components.html_escape community.slug) post_mods_html
  in

  let content = Printf.sprintf "
    <div class='flex flex-col lg:flex-row gap-6 items-start'>
        <div class='w-full lg:w-1/4 hidden lg:block self-start sticky top-20'>
            %s
        </div>
        <div class='w-full lg:w-2/4 min-w-0'>
            <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-6 mb-6'>
                <div class='mb-4'>
                    <a href='/c/%s' class='text-sm font-bold text-[#0D9488] hover:underline'>/c/%s</a>
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
                <div>%s</div>
            </div>
        </div>
        <div class='w-80 hidden lg:flex flex-col gap-6 self-start sticky top-20 h-[calc(100vh-5rem)] overflow-y-auto pb-8 [&::-webkit-scrollbar]:w-1.5 [&::-webkit-scrollbar-track]:bg-transparent [&::-webkit-scrollbar-thumb]:bg-transparent hover:[&::-webkit-scrollbar-thumb]:bg-gray-300 [&::-webkit-scrollbar-thumb]:rounded-full'>
            %s
        </div>
    </div>"
    (Components.left_sidebar ?user ~moderated_communities user_communities)
    post.community_slug post.community_slug
    (Components.render_author ~mod_usernames ~admin_usernames post.username) (Components.time_ago post.created_at) (delete_post_btn ^ ban_post_btn)
    post.title
    link_content post_content voting_pill
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
      "<a href='/settings' class='inline-flex items-center gap-1.5 mt-3 bg-[#0D9488] text-white text-sm font-semibold px-4 py-1.5 rounded-md hover:bg-teal-700 transition shadow-sm'>✏️ Edit Profile</a>"
    | _ -> ""
  in
  (* Admin Panel: only render for the logged-in admin on their own profile.
     Gating on both own-profile AND is_admin prevents leaking the /admin route
     to non-admins even if they inspect another admin's profile page. *)
  let admin_panel_btn =
    match user with
    | Some logged_in when logged_in = username && is_admin ->
      "<a href='/admin' class='inline-flex items-center gap-1.5 mt-3 bg-red-600 text-white text-sm font-semibold px-4 py-1.5 rounded-md hover:bg-red-700 transition shadow-sm'>🛡️ Admin Panel</a>"
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
      "text-[#0D9488] border-b-2 border-[#0D9488] pb-3 text-sm font-medium"
    else
      "text-gray-500 hover:text-gray-700 border-b-2 border-transparent hover:border-gray-300 pb-3 text-sm font-medium transition-colors"
  in
  let tab_nav = Printf.sprintf "
    <div class='flex items-center gap-6 border-b border-gray-200 mb-6'>
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
          <div class='bg-white border border-gray-200 rounded-xl shadow-sm p-4 mb-4'>
              <div class='flex items-center gap-3 text-xs text-gray-400 mb-2'>
                  <span>%s</span>
                  <span class='text-gray-300'>·</span>
                  <span class='font-medium text-gray-500'>%d points</span>
              </div>
              <p class='text-sm text-gray-800 leading-relaxed'>%s</p>
              <a href='/p/%d' class='text-xs text-[#0D9488] hover:underline mt-2 inline-block'>&#8618; Commented on: %s</a>
          </div>" created_at score content post_id post_title
      ) user_comments)
  in

  let feed_html =
    if active_tab = "comments" then comments_html else posts_html
  in

  let content = Printf.sprintf "
    <div class='max-w-4xl mx-auto mt-8'>
        <div class='bg-white p-8 rounded-lg shadow-sm border border-gray-200 mb-8'>
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

            <div class='mt-6 bg-gray-50 p-4 rounded-md border border-gray-100 text-gray-800 whitespace-pre-wrap text-sm'>%s</div>

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
    <div class='max-w-2xl mx-auto bg-white p-8 rounded-lg shadow-sm border border-gray-200 mt-8'>
        <h1 class='text-3xl font-extrabold text-gray-900 mb-8'>Account Settings</h1>

        <div class='mb-10 pb-8 border-b border-gray-200'>
            <h2 class='text-xl font-bold text-gray-800 mb-4'>Profile Information</h2>
            <form action='/settings' method='POST' class='space-y-6'>
                %s
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Avatar URL (Optional)</label>
                    <input type='url' name='avatar_url' value='%s' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border' placeholder='https://example.com/my-face.jpg'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Bio</label>
                    <textarea name='bio' rows='4' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border' placeholder='Tell the community a bit about yourself...'>%s</textarea>
                </div>
                <button type='submit' class='bg-[#0D9488] text-white font-bold py-2 px-6 rounded hover:bg-teal-700 transition shadow-sm'>Save Profile</button>
            </form>
        </div>

        <div class='mb-10 pb-8 border-b border-gray-200'>
            <h2 class='text-xl font-bold text-gray-800 mb-4'>Change Password</h2>
            <form action='/settings/password' method='POST' class='space-y-4'>
                %s
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Current Password</label>
                    <input type='password' name='old_password' required class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>New Password</label>
                    <input type='password' name='new_password' required minlength='8' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
                </div>
                <div>
                    <label class='block text-sm font-medium text-gray-700 mb-1'>Confirm New Password</label>
                    <input type='password' name='confirm_password' required minlength='8' class='block w-full rounded-md border-gray-300 focus:border-[#0D9488] focus:ring-1 focus:ring-[#0D9488]/20 focus:outline-none p-2 border'>
                </div>
                <button type='submit' class='bg-gray-800 text-white font-bold py-2 px-6 rounded hover:bg-gray-900 transition shadow-sm'>Update Password</button>
            </form>
        </div>

        <div class='mb-10 pb-8 border-b border-gray-200'>
            <h2 class='text-xl font-bold text-gray-900 mb-2'>Data Portability (GDPR Art. 20)</h2>
            <p class='text-gray-600 text-sm mb-4'>Download a complete machine-readable copy (JSON) of your personal data, including your profile information, posts, and comments.</p>
            <a href='/export-data' class='inline-block bg-teal-50 text-teal-700 border border-teal-200 font-bold py-2 px-6 rounded hover:bg-teal-100 transition shadow-sm'>↓ Download My Data</a>
        </div>

        <div class='mt-8 pt-6 border border-red-200 bg-red-50 p-6 rounded-lg'>
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
    let bg_color = if n.is_read then "bg-white" else "bg-teal-50 border-l-4 border-[#0D9488]" in

    let len = String.length n.message in
    let is_post = len >= 5 && String.sub n.message (len - 5) 5 = "post." in
    let icon = if is_post then "📝" else "💬" in

    Printf.sprintf "
    <a href='/p/%d' onclick=\"this.classList.remove('bg-teal-50', 'border-l-4', 'border-[#0D9488]'); this.classList.add('bg-white');\" class='block %s p-4 rounded-lg border border-gray-200 hover:shadow-md transition mb-3'>
        <div class='flex items-center'>
            <div class='text-2xl mr-4'>%s</div>
            <div>
                <p class='text-gray-900 font-medium'>%s</p>
                <p class='text-xs text-gray-500 mt-1'>%s</p>
            </div>
        </div>
    </a>" n.post_id bg_color icon (Components.html_escape n.message) (Components.time_ago n.created_at)
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
    Printf.sprintf "<a href='/c/%s' class='block p-4 bg-white border border-gray-200 rounded-lg hover:border-[#0D9488] mb-3 shadow-sm'><h3 class='font-bold text-lg text-gray-900'>%s</h3><p class='text-xs text-[#0D9488] mb-1'>/c/%s</p><p class='text-gray-700 text-sm'>%s</p></a>"
      (Components.html_escape a.slug) (Components.html_escape a.name)
      (Components.html_escape a.slug) (Components.html_escape (Option.value ~default:"No description" a.description))
  in

  let render_user (_, username, _, bio, avatar) =
    let eu = Components.html_escape username in
    let avatar_html = match avatar with
      | Some url when url <> "" -> Printf.sprintf "<img src='%s' class='w-12 h-12 rounded-full object-cover mr-4'>" (Components.html_escape url)
      | _ -> Printf.sprintf "<div class='w-12 h-12 bg-teal-100 rounded-full flex items-center justify-center text-[#0D9488] font-bold mr-4'>%s</div>" (String.sub username 0 1 |> String.uppercase_ascii)
    in
    Printf.sprintf "<a href='/u/%s' class='block p-4 bg-white border border-gray-200 rounded-lg hover:border-[#0D9488] mb-3 shadow-sm flex items-center'>%s <div><h3 class='font-bold text-lg text-gray-900'>u/%s</h3><p class='text-sm text-gray-500'>%s</p></div></a>"
      eu avatar_html eu (Components.html_escape (Option.value ~default:"" bio))
  in

  let render_search_comment (_, content, username, created_at, post_id, score) =
    Printf.sprintf "<div class='p-4 bg-white border border-gray-200 rounded-lg mb-3 shadow-sm'><div class='text-xs text-gray-500 mb-2'>%s • %s • Score: %d</div><div class='text-gray-800 text-sm mb-3'>%s</div><a href='/p/%d' class='text-xs font-bold text-[#0D9488] hover:underline bg-teal-50 px-2 py-1 rounded'>Go to Thread &rarr;</a></div>"
      (Components.render_author ~admin_usernames username) (Components.time_ago created_at) score (Components.html_escape content) post_id
  in

  let content_html, has_next =
    match active_tab with
    | "communities" ->
        if communities = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-lg border border-dashed border-gray-300'>No communities found for this search.</div>", false)
        else (String.concat "\n" (List.map render_community communities), List.length communities = 20)
    | "people" ->
        if users = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-lg border border-dashed border-gray-300'>No people found for this search.</div>", false)
        else (String.concat "\n" (List.map render_user users), List.length users = 20)
    | "comments" ->
        if comments = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-lg border border-dashed border-gray-300'>No comments found for this search.</div>", false)
        else (String.concat "\n" (List.map render_search_comment comments), List.length comments = 20)
    | _ ->
        if posts = [] then ("<div class='text-center py-16 text-gray-500 bg-white rounded-lg border border-dashed border-gray-300'>No posts found for this search.</div>", false)
        else (String.concat "\n" (List.map (Components.render_post ~admin_usernames request user_votes) posts), List.length posts = 20)
  in

  let get_tab_class tab_name =
    if tab_name = active_tab then
      "font-bold text-sm tracking-wide uppercase text-[#0D9488] pb-3 border-b-2 border-[#0D9488]"
    else
      "font-bold text-sm tracking-wide uppercase text-gray-500 hover:text-[#0D9488] pb-3 border-b-2 border-transparent hover:border-[#0D9488] transition"
  in

  let tabs_html = Printf.sprintf "
    <div class='bg-gray-50 pt-4 mb-6 border-b border-gray-200 flex space-x-8 overflow-x-auto'>
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

  let prev_btn = if current_page <= 1 then "" else Printf.sprintf "<a href='/search?q=%s&t=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50'>&larr; Prev</a>" query active_tab (current_page - 1) in
  let next_btn = if not has_next then "" else Printf.sprintf "<a href='/search?q=%s&t=%s&page=%d' class='bg-white border border-gray-300 text-gray-700 px-4 py-2 rounded font-bold hover:bg-gray-50'>Next &rarr;</a>" query active_tab (current_page + 1) in

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

(* Two-part structure: plain-English human summary up top, then full technical/legal
   disclosure beneath. Dual audience (users + lawyers/DPAs) without burying either. *)
let privacy_page ?user request =
  let content = "
    <div class='max-w-2xl mx-auto mt-10 mb-16'>

      <h1 class='text-3xl font-extrabold text-gray-900 mb-2'>Privacy Policy</h1>
      <p class='text-sm text-gray-400 mb-10'>Last updated: March 2026 &mdash; Earde Network (provisional Data Controller)</p>

      <!-- PART 1: Human-readable summary. Intentionally blunt. -->
      <div class='bg-[#F0FDF9] border border-[#0D9488]/20 rounded-xl p-6 mb-12'>
        <p class='text-xs font-bold text-[#0D9488] uppercase tracking-widest mb-4'>Part 1 &mdash; The Short Version (Plain English)</p>
        <div class='space-y-4 text-gray-800 leading-relaxed'>
          <p class='text-lg font-semibold'>Here is every piece of personal information Earde collects about you:</p>
          <ul class='list-none space-y-2 text-base'>
            <li class='flex items-start gap-2'><span class='text-[#0D9488] font-bold mt-0.5'>&#10003;</span><span><strong>Your username.</strong> Public. It is your identity on the platform.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#0D9488] font-bold mt-0.5'>&#10003;</span><span><strong>Your email address.</strong> Private. Used only to verify your account and recover your password.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#0D9488] font-bold mt-0.5'>&#10003;</span><span><strong>Your password.</strong> We never see it. It is immediately hashed with Argon2id and the original is discarded.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#0D9488] font-bold mt-0.5'>&#10003;</span><span><strong>Your posts, comments, and votes.</strong> The forum content you choose to create.</span></li>
            <li class='flex items-start gap-2'><span class='text-[#0D9488] font-bold mt-0.5'>&#10003;</span><span><strong>Your IP address, transiently.</strong> Held in memory for rate limiting. Never written to the database.</span></li>
          </ul>
          <div class='border-t border-[#0D9488]/20 pt-4 mt-4 space-y-2 text-sm text-gray-700'>
            <p><strong>No advertising. No tracking pixels. No third-party analytics. No data brokers. No behavioural profiling. No selling your data. Ever.</strong></p>
            <p>We set exactly one cookie: a signed session cookie that keeps you logged in. It contains no tracking identifier. It is deleted when you log out. Because it is strictly necessary for the service to function, we are not legally required to ask for your consent to set it &mdash; which is why there is no cookie banner.</p>
            <p>You can <a href='/export-data' class='text-[#0D9488] underline hover:text-teal-700 font-medium'>download everything we have about you</a> as JSON at any time. You can <a href='/settings' class='text-[#0D9488] underline hover:text-teal-700 font-medium'>delete your account</a> instantly. When you do, your personal data is wiped from our database within seconds.</p>
            <p>That is it. No fine print that contradicts any of the above.</p>
          </div>
        </div>
      </div>

      <!-- PART 2: Technical and legal specification. For lawyers, auditors, DPAs. -->
      <p class='text-xs font-bold text-gray-500 uppercase tracking-widest mb-6'>Part 2 &mdash; Technical &amp; Legal Specification</p>

      <div class='space-y-10 text-gray-700 leading-relaxed'>

        <!-- Art. 13(1)(a-b) GDPR -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>1. Data Controller (Art. 13(1)(a))</h2>
          <p>The data controller is <strong>Earde Network</strong> (provisional). Contact for all data protection matters: <strong>privacy@earde.eu</strong>. No DPO is appointed at this stage; Earde Network does not carry out large-scale systematic monitoring and does not process special-category data (Art. 37 GDPR threshold not met).</p>
        </section>

        <!-- Art. 13(1)(c-d) GDPR: legal basis table -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>2. Personal data processed and legal basis (Art. 6 &amp; 13(1)(c))</h2>
          <p class='mb-4 text-sm'>Every category of personal data, its technical representation, retention period, and the applicable Art. 6(1) legal basis:</p>
          <div class='overflow-x-auto'>
            <table class='w-full text-sm border border-gray-200 rounded-lg overflow-hidden'>
              <thead class='bg-gray-50 text-gray-600 font-semibold'>
                <tr>
                  <th class='text-left px-4 py-2 border-b border-gray-200'>Data category</th>
                  <th class='text-left px-4 py-2 border-b border-gray-200'>Storage</th>
                  <th class='text-left px-4 py-2 border-b border-gray-200'>Retention</th>
                  <th class='text-left px-4 py-2 border-b border-gray-200'>Legal basis</th>
                </tr>
              </thead>
              <tbody class='divide-y divide-gray-100'>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Username</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>users.username</code> (plaintext; public by design)</td>
                  <td class='px-4 py-2 text-gray-500'>Until account deletion; then replaced with <code class='bg-gray-100 px-1 rounded'>[deleted_&lt;id&gt;]</code></td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Email address</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>users.email</code> (plaintext; never exposed in UI)</td>
                  <td class='px-4 py-2 text-gray-500'>Until account deletion; then replaced with <code class='bg-gray-100 px-1 rounded'>deleted_&lt;id&gt;@earde.local</code></td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Password</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>users.password_hash</code> (Argon2id; raw password never persisted)</td>
                  <td class='px-4 py-2 text-gray-500'>Until account deletion; then zeroed</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Posts &amp; comments</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>posts</code>, <code class='bg-gray-100 px-1 rounded'>comments</code></td>
                  <td class='px-4 py-2 text-gray-500'>Indefinite as community content; authorship anonymised on account deletion</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Votes</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>post_votes</code>, <code class='bg-gray-100 px-1 rounded'>comment_votes</code></td>
                  <td class='px-4 py-2 text-gray-500'>Cascade-deleted on account deletion</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Bio &amp; avatar URL (optional)</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>users.bio</code>, <code class='bg-gray-100 px-1 rounded'>users.avatar_url</code></td>
                  <td class='px-4 py-2 text-gray-500'>Until changed or account deletion</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(a) &mdash; consent (freely given, specific, withdrawable)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>IP address</td>
                  <td class='px-4 py-2 text-gray-500'>Dream framework in-process memory only; used for sliding-window rate limiting</td>
                  <td class='px-4 py-2 text-gray-500'>Ephemeral; evicted on server restart, never written to database or logs</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(f) &mdash; legitimate interest (abuse prevention)</td>
                </tr>
                <tr class='hover:bg-gray-50'>
                  <td class='px-4 py-2 font-medium'>Password reset token</td>
                  <td class='px-4 py-2 text-gray-500'>PostgreSQL <code class='bg-gray-100 px-1 rounded'>users.reset_token</code></td>
                  <td class='px-4 py-2 text-gray-500'>Hard-expiry: 1 hour from issuance</td>
                  <td class='px-4 py-2 text-gray-500'>Art. 6(1)(b) &mdash; performance of contract</td>
                </tr>
              </tbody>
            </table>
          </div>
          <p class='mt-4 text-sm text-gray-500'>No special-category data (Art. 9) is collected or processed. No data is transferred to third countries outside the EEA. No automated decision-making or profiling (Art. 22) takes place.</p>
        </section>

        <!-- ePrivacy Directive 2002/58/EC, Recital 25 -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>3. Cookie and session architecture</h2>
          <p class='mb-3'>Earde sets <strong>exactly one cookie</strong>: a cryptographically-signed session cookie issued by the Dream web framework. The cookie is HMAC-SHA256 signed using a 256-bit secret key configured via the <code class='bg-gray-100 px-1 rounded text-sm'>DREAM_SECRET</code> environment variable. It carries only an opaque session identifier; the session payload (user ID, username, admin flag) is held server-side in process memory and is never exposed to the client.</p>
          <p class='mb-3'>This cookie is <strong>strictly necessary</strong> for the service to function: without it, login state cannot be maintained across requests. Under the ePrivacy Directive 2002/58/EC, Article 5(3) and Recital 25, strictly-necessary cookies are exempt from the prior-consent requirement. This is the legal basis for the absence of a cookie consent banner on Earde.</p>
          <p>The cookie is cleared immediately on logout (<code class='bg-gray-100 px-1 rounded text-sm'>Dream.invalidate_session</code>). It does not persist across devices or browser profiles. No third-party cookies are set by Earde or any resource it loads.</p>
        </section>

        <!-- Art. 17 GDPR: right to erasure — technical implementation -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>4. Right to Erasure &mdash; technical implementation (Art. 17)</h2>
          <p class='mb-3'>Account deletion is handled by <code class='bg-gray-100 px-1 rounded text-sm'>Db.anonymize_user</code>, which executes the following atomic SQL update:</p>
          <pre class='bg-gray-900 text-green-300 rounded-lg p-4 text-sm overflow-x-auto leading-relaxed'>UPDATE users
  SET username      = \'[deleted_\' || id || \']\',
      email         = \'deleted_\' || id || \'@earde.local\',
      password_hash = \'\'
  WHERE id = $1</pre>
          <p class='mt-3 mb-3'>Downstream data (votes, memberships, moderator roles, community bans, notifications) is removed by PostgreSQL <code class='bg-gray-100 px-1 rounded text-sm'>ON DELETE CASCADE</code> foreign-key constraints, which fire within the same transaction.</p>
          <p class='mb-3'><strong>Why anonymisation rather than hard-delete?</strong> Hard-deleting a user row would create orphaned comment and post rows, breaking thread coherence and violating referential integrity. Anonymisation satisfies Art. 17 because the resulting tokens (<code class='bg-gray-100 px-1 rounded text-sm'>[deleted_42]</code>, <code class='bg-gray-100 px-1 rounded text-sm'>deleted_42@earde.local</code>) cannot be re-linked to the original natural person without additional information that is simultaneously destroyed &mdash; meeting the pseudonymisation standard of Art. 4(5) and the &ldquo;no longer personal data&rdquo; threshold of Recital 26.</p>
          <p>The post-deletion session is immediately invalidated (<code class='bg-gray-100 px-1 rounded text-sm'>Dream.invalidate_session</code>), ensuring no authenticated request can be made on behalf of the deleted identity.</p>
          <p class='mt-3'><span class='text-[#0D9488] font-medium'>In-product shortcut:</span> <a href='/settings' class='text-[#0D9488] underline hover:text-teal-700'>Settings &rarr; Danger Zone &rarr; Delete account</a>.</p>
        </section>

        <!-- Art. 20 GDPR: data portability — technical implementation -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>5. Data Portability &mdash; technical implementation (Art. 20)</h2>
          <p class='mb-3'>The <code class='bg-gray-100 px-1 rounded text-sm'>GET /export-data</code> endpoint returns a <code class='bg-gray-100 px-1 rounded text-sm'>Content-Disposition: attachment</code> JSON document containing the full data set held against the authenticated user: profile fields (username, bio, avatar_url, joined_at), all posts (id, title, url, content, community_slug, created_at, score), and all comments (id, content, created_at, post_id, post_title, score). The format is structured JSON &mdash; machine-readable and interoperable &mdash; satisfying the portability format requirement of Art. 20(1). The endpoint requires an authenticated session; unauthenticated requests receive a 401 redirect.</p>
          <p class='mt-3'><span class='text-[#0D9488] font-medium'>In-product shortcut:</span> <a href='/export-data' class='text-[#0D9488] underline hover:text-teal-700'>Settings &rarr; Export your data</a>.</p>
        </section>

        <!-- Art. 15-21 GDPR: full rights enumeration -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>6. Data subject rights (Arts. 15&ndash;21)</h2>
          <ul class='space-y-3'>
            <li><span class='font-semibold text-gray-900'>Access (Art. 15)</span> &mdash; Request a full copy of your data. <span class='text-[#0D9488]'>In-product: <a href='/export-data' class='underline hover:text-teal-700'>Export data</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Rectification (Art. 16)</span> &mdash; Correct inaccurate data at any time. <span class='text-[#0D9488]'>In-product: <a href='/settings' class='underline hover:text-teal-700'>Settings &rarr; Edit profile</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Erasure (Art. 17)</span> &mdash; Delete your account and anonymise your personal data immediately. <span class='text-[#0D9488]'>In-product: <a href='/settings' class='underline hover:text-teal-700'>Settings &rarr; Delete account</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Restriction (Art. 18)</span> &mdash; Request restriction of processing pending a dispute. Contact privacy@earde.eu.</li>
            <li><span class='font-semibold text-gray-900'>Portability (Art. 20)</span> &mdash; Receive your data in a structured, machine-readable format. <span class='text-[#0D9488]'>In-product: <a href='/export-data' class='underline hover:text-teal-700'>Export data</a>.</span></li>
            <li><span class='font-semibold text-gray-900'>Object (Art. 21)</span> &mdash; Object to processing based on legitimate interest (Art. 6(1)(f)). We will cease unless overriding legitimate grounds are demonstrated. Contact privacy@earde.eu.</li>
          </ul>
          <p class='mt-4 text-sm'>To exercise any right not available via the product interface, write to <strong>privacy@earde.eu</strong>. We will respond within <strong>30 days</strong> per Art. 12(3), at no charge per Art. 12(5).</p>
        </section>

        <!-- Art. 32 GDPR: security -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>7. Security measures (Art. 32)</h2>
          <ul class='list-disc list-inside space-y-2 text-sm'>
            <li><strong>Argon2id</strong> for all password hashes (OWASP recommended; PHC winner). Raw passwords are never logged or persisted.</li>
            <li><strong>CSRF tokens</strong> on every state-mutating form (Dream framework built-in).</li>
            <li><strong>HTML output escaping</strong> on all user-supplied strings before template interpolation, preventing stored and reflected XSS.</li>
            <li><strong>URL validation</strong> blocking <code class='bg-gray-100 px-1 rounded'>javascript:</code> and <code class='bg-gray-100 px-1 rounded'>data:</code> schemes in user-supplied link targets.</li>
            <li><strong>Rate limiting</strong> (5 attempts / 60 seconds, sliding window) on signup, login, and password reset endpoints.</li>
            <li><strong>TLS</strong> enforced in transit. The session cookie is issued with <code class='bg-gray-100 px-1 rounded'>Secure</code>, <code class='bg-gray-100 px-1 rounded'>HttpOnly</code>, and <code class='bg-gray-100 px-1 rounded'>SameSite=Strict</code> attributes by the Dream framework.</li>
            <li><strong>Server-side re-authorisation</strong> on all privilege-sensitive mutations (moderator actions, admin actions) &mdash; no trust placed in client-supplied role claims.</li>
          </ul>
        </section>

        <!-- Art. 13(2)(d) GDPR: mandatory supervisory authority disclosure -->
        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>8. Right to lodge a supervisory authority complaint (Art. 13(2)(d))</h2>
          <p>Without prejudice to any other administrative or judicial remedy, you have the right to lodge a complaint with the supervisory authority in the EU Member State of your habitual residence, place of work, or place of the alleged infringement, if you consider that the processing of your personal data infringes the GDPR. A full directory of EU/EEA supervisory authorities is maintained at <strong>edpb.europa.eu</strong>.</p>
        </section>

        <section>
          <h2 class='text-lg font-bold text-gray-900 mb-3 pb-1 border-b border-gray-200'>9. Changes to this policy</h2>
          <p>Material changes will be communicated to registered users via a platform notification at least 14 days before they take effect. The &ldquo;Last updated&rdquo; date at the top of this page always reflects the current version. Continued use of Earde after the effective date constitutes acceptance of the revised policy.</p>
        </section>

      </div>
    </div>"
  in
  Components.layout ?user ~request ~title:"Privacy Policy" content

(* About page: opinionated manifesto in the Lobsters/HN tradition.
   Dense prose, no marketing fluff — explains the why, not just the what. *)
let about_page ?user request =
  let content = "
    <div class='max-w-2xl mx-auto mt-10 mb-16'>

      <h1 class='text-3xl font-extrabold text-gray-900 mb-8'>About Earde</h1>

      <div class='space-y-8 text-gray-700 leading-relaxed'>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>What is Earde?</h2>
          <p class='mb-3'>Earde is a community forum and link aggregator built as a fast, open-source, European alternative to Reddit. It is structured around <em>communities</em> &mdash; self-governing spaces, each with its own moderators, rules, and culture. There is no algorithmic feed, no recommendation engine, and no engagement optimisation. Posts are ranked by community votes. The most recent activity surfaces to the top. That is the entire algorithm.</p>
          <p>The name comes from the ancient Greek <em>koin&oacute;n</em> (&kappa;&omicron;&iota;&nu;&#972;&nu;) &mdash; a league of communities acting in common. That is the model: sovereign communities, federated under shared rules of conduct, with no central authority dictating what you see or think.</p>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>Why we built it</h2>
          <p class='mb-3'>Reddit was a great idea that survived contact with venture capital. Over a decade it accumulated dark patterns: infinite scroll, autoplay video, feed manipulation, third-party ad networks, aggressive data collection, and a 2023 API shutdown that killed every serious third-party client. The company that built it is now publicly traded and answers to shareholders, not users.</p>
          <p class='mb-3'>We are not interested in that trajectory. Earde is built for people who want a text-centric discussion forum that does not treat them as inventory to be monetised. It is built in Europe, hosted on European bare-metal servers, and designed from the first line of code to be compliant with the GDPR &mdash; not as an afterthought, but as an architectural constraint.</p>
          <p>We are also making a small technical statement. Earde is written in <strong>OCaml</strong>, a statically-typed functional language developed at INRIA in France. OCaml has been powering critical infrastructure in finance, verification, and compilers for decades. It deserves a turn on the web.</p>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>What we believe</h2>
          <ul class='list-disc list-inside space-y-3'>
            <li><strong>Privacy is a default, not a setting.</strong> We collect your username and email. Nothing else. See the <a href='/privacy' class='text-[#0D9488] underline hover:text-teal-700'>Privacy Policy</a> for the full technical disclosure.</li>
            <li><strong>Communities should govern themselves.</strong> Each community sets its own rules and elects its own moderators. Platform-wide rules exist only to prevent serious harm, not to enforce a particular culture.</li>
            <li><strong>Text is a feature, not a limitation.</strong> Earde is designed for reading and writing. Images, video, and infinite scroll are not on the roadmap.</li>
            <li><strong>No algorithmic mediation.</strong> You see what your communities voted on, in the order they voted on it. We do not decide what is important for you.</li>
            <li><strong>Open source is non-negotiable.</strong> You can audit every line of code, run your own instance, and fork the project. There are no backdoors, no proprietary modules, and no dark-pattern UI patterns hidden in a closed binary.</li>
          </ul>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>Community guidelines</h2>
          <p class='mb-3'>Earde communities are self-governing within the following platform-wide rules. These exist to protect users, not to police opinion.</p>
          <ul class='list-disc list-inside space-y-2'>
            <li>No harassment, targeted abuse, or threats directed at individuals or groups.</li>
            <li>No content that is illegal under the law of the European Union or the jurisdiction of the hosting country.</li>
            <li>No spam, unsolicited commercial solicitation, or coordinated inauthentic behaviour.</li>
            <li>No impersonation of real persons, organisations, or public figures.</li>
            <li>No content that sexualises or endangers minors.</li>
          </ul>
          <p class='mt-3'>Violations of these rules may result in content removal, community ban, or global suspension at moderator or administrator discretion. Community-specific rules are set by each community&rsquo;s moderators and displayed in the community sidebar.</p>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>Who runs this?</h2>
          <p>Earde is operated by <strong>Earde Network</strong> (provisional legal entity, European Union). The platform is hosted on bare-metal servers within the EEA. No user data is transferred to third countries. For legal and regulatory correspondence: <strong>legal@earde.eu</strong>.</p>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>Open source</h2>
          <p>The full source code for the Earde platform is publicly available. It is written in OCaml 5 using the Dream web framework and PostgreSQL. The codebase is intentionally small and readable &mdash; a few large, cohesive files rather than dozens of micro-modules. You can read it, run it, and improve it. Pull requests are welcome. Bug reports are appreciated. Forks are expected.</p>
        </section>

        <section>
          <h2 class='text-base font-bold text-gray-900 uppercase tracking-wide mb-3'>Contact</h2>
          <p>General enquiries: <strong>hello@earde.eu</strong><br>
             Privacy &amp; data protection: <strong>privacy@earde.eu</strong> &mdash; or see the <a href='/privacy' class='text-[#0D9488] underline hover:text-teal-700'>Privacy Policy</a><br>
             Abuse &amp; safety reports: <strong>abuse@earde.eu</strong><br>
             Legal &amp; regulatory: <strong>legal@earde.eu</strong></p>
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
      <div class='bg-white border border-gray-200 rounded-2xl shadow-sm p-8 max-w-md w-full text-center'>
        %s
        <h1 class='text-2xl font-bold text-gray-900 mb-2'>%s</h1>
        <p class='text-gray-500 mb-6'>%s</p>
        <a href='%s' class='inline-block bg-[#0D9488] text-white rounded-lg px-6 py-2.5 font-medium hover:bg-teal-700 transition-colors'>Go Back</a>
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
              <a href='/u/%s' class='font-medium text-gray-900 hover:text-[#0D9488] hover:underline'>u/%s</a>
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
        <a href='/earde-hq-dashboard' class='text-sm text-[#0D9488] hover:underline font-bold'>📊 KPI Dashboard &rarr;</a>
      </div>
      <div class='bg-white rounded-lg shadow-sm border border-gray-200 p-6'>
        <h2 class='text-lg font-bold text-gray-800 mb-1'>Globally Banned Users</h2>
        <p class='text-xs text-gray-500 mb-4'>These accounts are blocked from logging in and posting anywhere on Earde.</p>
        %s
      </div>
    </div>" banned_rows
  in
  Components.layout ?user ~request ~title:"Admin Dashboard" content

(* Standalone HTML — intentionally outside Components.layout to prevent nav/JS
   assets from loading on an admin-only internal page that needs no public shell. *)
let hq_dashboard_page (((v1, v7, v30), (uv1, uv7, uv30)), (u1, u7, u30), (c1, c7, c30), (a1, a7, a30)) =
  Printf.sprintf "
  <!DOCTYPE html>
  <html lang='en'>
  <head>
      <meta charset='UTF-8'>
      <meta name='viewport' content='width=device-width, initial-scale=1.0'>
      <meta http-equiv='refresh' content='60'> <title>Earde HQ - Mission Control</title>
      <script src='https://cdn.tailwindcss.com'></script>
  </head>
  <body class='bg-gray-950 text-green-400 font-mono min-h-screen p-8'>
      <div class='max-w-7xl mx-auto'>
          <header class='flex justify-between items-end border-b border-green-900 pb-4 mb-8'>
              <div>
                  <h1 class='text-4xl font-bold tracking-tighter text-white'>Earde <span class='text-green-500'>SYS.CORE</span></h1>
                  <p class='text-green-700 text-sm mt-1'>live telemetry // UPDATED EVERY 60s</p>
              </div>
              <div class='text-right'>
                  <div class='text-3xl font-bold text-white'>%d</div>
                  <div class='text-xs text-green-700'>TOTAL MAU (30 DAYS)</div>
              </div>
          </header>

          <div class='grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-6'>

              <div class='bg-gray-900 border border-green-900 p-6 rounded-lg shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                  <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>PAGE VIEWS</h2>
                  <div class='mb-4'>
                      <div class='text-4xl font-bold text-white'>%d</div>
                      <div class='text-xs text-green-700'>TODAY (24H)</div>
                      <div class='text-sm text-green-500 mt-1'>%d <span class='text-green-800 text-xs'>unique visitors</span></div>
                  </div>
                  <div class='flex justify-between border-t border-green-900 pt-3'>
                      <div>
                          <div class='text-lg font-bold text-gray-300'>%d</div>
                          <div class='text-[10px] text-green-800'>VIEWS 7D</div>
                          <div class='text-xs text-green-600'>%d UV</div>
                      </div>
                      <div class='text-right'>
                          <div class='text-lg font-bold text-gray-300'>%d</div>
                          <div class='text-[10px] text-green-800'>VIEWS 30D</div>
                          <div class='text-xs text-green-600'>%d UV</div>
                      </div>
                  </div>
              </div>

              <div class='bg-gray-900 border border-green-900 p-6 rounded-lg shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                  <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>NEW SIGNUPS</h2>
                  <div class='mb-4'><div class='text-4xl font-bold text-white'>%d</div><div class='text-xs text-green-700'>TODAY (24H)</div></div>
                  <div class='flex justify-between border-t border-green-900 pt-3'>
                      <div><div class='text-lg font-bold text-gray-300'>%d</div><div class='text-[10px] text-green-800'>WEEK (7D)</div></div>
                      <div class='text-right'><div class='text-lg font-bold text-gray-300'>%d</div><div class='text-[10px] text-green-800'>MONTH (30D)</div></div>
                  </div>
              </div>

              <div class='bg-gray-900 border border-green-900 p-6 rounded-lg shadow-[0_0_15px_rgba(34,197,94,0.1)]'>
                  <h2 class='text-green-600 text-sm font-bold mb-4 tracking-widest'>CONTENT LIQUIDITY</h2>
                  <div class='mb-4'><div class='text-4xl font-bold text-white'>%d</div><div class='text-xs text-green-700'>TODAY (24H)</div></div>
                  <div class='flex justify-between border-t border-green-900 pt-3'>
                      <div><div class='text-lg font-bold text-gray-300'>%d</div><div class='text-[10px] text-green-800'>WEEK (7D)</div></div>
                      <div class='text-right'><div class='text-lg font-bold text-gray-300'>%d</div><div class='text-[10px] text-green-800'>MONTH (30D)</div></div>
                  </div>
              </div>

              <div class='bg-gray-900 border border-green-900 p-6 rounded-lg shadow-[0_0_15px_rgba(34,197,94,0.1)] relative overflow-hidden'>
                  <div class='absolute top-0 right-0 w-16 h-16 bg-green-500 opacity-10 rounded-bl-full'></div>
                  <h2 class='text-green-500 text-sm font-bold mb-4 tracking-widest'>ACTIVE CONTRIBUTORS</h2>
                  <div class='mb-4'>
                      <div class='text-4xl font-bold text-white'>%d <span class='text-sm text-green-600 font-normal'>DAU</span></div>
                      <div class='text-xs text-green-700'>TODAY (24H)</div>
                  </div>
                  <div class='flex justify-between border-t border-green-900 pt-3'>
                      <div><div class='text-lg font-bold text-green-400'>%d <span class='text-[10px] text-green-700'>WAU</span></div><div class='text-[10px] text-green-800'>WEEK (7D)</div></div>
                      <div class='text-right'><div class='text-lg font-bold text-green-400'>%d <span class='text-[10px] text-green-700'>MAU</span></div><div class='text-[10px] text-green-800'>MONTH (30D)</div></div>
                  </div>
              </div>

          </div>
      </div>
  </body>
  </html>"
  a30
  v1 uv1 v7 uv7 v30 uv30
  u1 u7 u30
  c1 c7 c30
  a1 a7 a30
