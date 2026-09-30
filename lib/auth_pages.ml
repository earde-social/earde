let signup_form ?user:_ ?error ?turnstile_site_key request =
  let csrf_token = Csrf_field.tag request in
  (* Server-authored messages only (never user input); rendered as the flat
     rejected notice above the card — no toast, no animation. *)
  let error_html =
    match error with
    | None -> Html.empty
    | Some msg ->
        Html.template
          "<p class='notice notice--rejected launch-auth__alert'>%s</p>" [ msg ]
  in
  (* Turnstile widget is rendered only when a site key is configured. The site key
     is public; still escape it as defense-in-depth. The challenge needs JS to
     solve, but the surrounding SSR form is unaffected when JS is off. *)
  let turnstile_widget, turnstile_script =
    match turnstile_site_key with
    | None -> (Html.empty, Html.empty)
    | Some key ->
        ( Html.template "<div class='cf-turnstile' data-sitekey='%s'></div>"
            [ Html.text key ],
          Html.static
            "<script \
             src='https://challenges.cloudflare.com/turnstile/v0/api.js' async \
             defer></script>" )
  in
  let content =
    Html.template
      "\n\
      \        <div class='auth'>\n\
      \          <div class='auth__head'>\n\
      \            <img class='auth__mark' src='/static/images/logo-mark.svg' \
       alt=''>\n\
      \            <h1 class='auth__title'>Create an account</h1>\n\
      \            <p class='auth__sub'>One account for every community on \
       Earde.</p>\n\
      \          </div>\n\
      \          %s\n\
      \          <form action='/signup' method='POST' class='auth__card'>\n\
      \            %s\n\n\
      \            <div class='field'>\n\
      \                <label class='label' for='su-username'>Username</label>\n\
      \                <div class='input-group'>\n\
      \                    <span class='input-group__prefix'>u/</span>\n\
      \                    <input class='input input--mono input--inset' \
       type='text' id='su-username' name='username' required>\n\
      \                </div>\n\
      \                <p class='hint'>3&#8211;30 characters</p>\n\
      \            </div>\n\n\
      \            <div class='field'>\n\
      \                <label class='label' for='su-email'>Email</label>\n\
      \                <input class='input input--inset' type='email' \
       id='su-email' name='email' required>\n\
      \            </div>\n\n\
      \            <div class='field'>\n\
      \                <label class='label' for='su-password'>Password</label>\n\
      \                <input class='input input--inset' type='password' \
       id='su-password' name='password' required>\n\
      \                <p class='hint'>At least 8 characters.</p>\n\
      \            </div>\n\n\
      \            <!-- Honeypot: positioned off-screen so humans never see or \
       fill it; a non-empty\n\
      \                 'website' on POST marks the submission as a bot and is \
       silently dropped. -->\n\
      \            <div \
       style='position:absolute;left:-9999px;top:-9999px;height:0;width:0;overflow:hidden' \
       aria-hidden='true'>\n\
      \                <label for='website'>Leave this field empty</label>\n\
      \                <input type='text' id='website' name='website' \
       tabindex='-1' autocomplete='off'>\n\
      \            </div>\n\n\
      \            <label class='check launch-auth__legal' for='privacy'>\n\
      \                <input id='privacy' name='privacy' type='checkbox' \
       required>\n\
      \                <span>I have read and accept the <a href='/privacy' \
       target='_blank'>Privacy Policy</a>.</span>\n\
      \            </label>\n\n\
      \            %s\n\n\
      \            <button type='submit' class='btn btn--primary btn--block \
       launch-auth__submit'>Create account</button>\n\
      \          </form>\n\
      \          %s\n\
      \          <p class='notice launch-auth__notice'>Maintainers: create \
       your account first, then <a href='/bring'>connect a project through \
       GitHub</a>.</p>\n\
      \          <p class='auth__foot'>Already have an account? <a \
       href='/login'>Log in &#8594;</a></p>\n\
      \        </div>"
      [ error_html; csrf_token; turnstile_widget; turnstile_script ]
  in
  Page_shell.launch_auth_page ~request ~page_class:"launch-signup"
    ~title:"Create an account" ~content ()

(* /login through the same launch wrapper. The
   form contract is unchanged — POST /login, Dream CSRF tag, 'identifier' and
   'password' names with their required flags, /forgot-password link — and no
   remember-me is added (unsupported). Failures still render through the
   legacy msg_page, untouched by this pass. ?user ignored as in signup_form. *)
let login_form ?user:_ request =
  let csrf_token = Csrf_field.tag request in
  let content =
    Html.template
      "\n\
      \        <div class='auth'>\n\
      \          <div class='auth__head'>\n\
      \            <img class='auth__mark' src='/static/images/logo-mark.svg' \
       alt=''>\n\
      \            <h1 class='auth__title'>Log in</h1>\n\
      \            <p class='auth__sub'>Reading is open to everyone. Log in to \
       post, vote and join communities.</p>\n\
      \          </div>\n\
      \          <form action='/login' method='POST' class='auth__card'>\n\
      \            %s\n\
      \            <div class='field'>\n\
      \                <label class='label' for='li-identifier'>Username or \
       email</label>\n\
      \                <input class='input input--inset' type='text' \
       id='li-identifier' name='identifier' required>\n\
      \            </div>\n\
      \            <div class='field launch-auth__field-last'>\n\
      \                <label class='label' for='li-password'>Password</label>\n\
      \                <input class='input input--inset' type='password' \
       id='li-password' name='password' required>\n\
      \            </div>\n\
      \            <div class='launch-auth__meta'>\n\
      \                <a href='/forgot-password' tabindex='-1'>Forgot \
       password?</a>\n\
      \            </div>\n\
      \            <button type='submit' class='btn btn--primary btn--block \
       launch-auth__submit'>Log in</button>\n\
      \          </form>\n\
      \          <p class='notice launch-auth__notice'>Maintaining an \
       open-source project? <a href='/bring'>Connect it through GitHub</a> \
       after logging in.</p>\n\
      \          <p class='auth__foot'>No account? <a href='/signup'>Create \
       one &#8594;</a></p>\n\
      \        </div>"
      [ csrf_token ]
  in
  Page_shell.launch_auth_page ~request ~page_class:"launch-login"
    ~title:"Log in" ~content ()

(* /forgot-password through the launch auth
   wrapper. The inner form is preserved verbatim — POST /forgot-password,
   Dream CSRF tag first, single 'email' field with its id/label/placeholder/
   required flags, and the legacy auth-form/auth-field/auth-input/auth-btn
   classes intact (skinned by the scoped "password recovery only" section of
   earde.css) — only the outer chrome moved. Heading and sub keep their exact
   factual wording: no copy may hint at whether an account exists (the POST's
   anti-enumeration contract lives in the handler and stays msg_page-rendered,
   untouched). noindex preserved from the legacy auth_page call. *)
let forgot_password_page request =
  let csrf_token = Csrf_field.tag request in
  let content =
    Html.template
      "\n\
      \        <div class='auth'>\n\
      \          <div class='auth__head'>\n\
      \            <img class='auth__mark' src='/static/images/logo-mark.svg' \
       alt=''>\n\
      \            <h1 class='auth__title'>Forgot password?</h1>\n\
      \            <p class='auth__sub'>Enter your email address and we'll \
       send you a reset link.</p>\n\
      \          </div>\n\
      \          <form action='/forgot-password' method='POST' \
       class='auth-form'>\n\
      \            %s\n\
      \            <div class='auth-field'>\n\
      \                <label class='auth-label' for='fp-email'>Email \
       address</label>\n\
      \                <input class='auth-input' type='email' id='fp-email' \
       name='email' required placeholder='you@example.com'>\n\
      \            </div>\n\
      \            <button type='submit' class='auth-btn'>Send reset \
       link</button>\n\
      \          </form>\n\
      \          <div class='auth-foot'><a href='/login' \
       class='auth-link'>Back to login</a></div>\n\
      \        </div>"
      [ csrf_token ]
  in
  Page_shell.launch_auth_page ~noindex:true ~request
    ~page_class:"launch-forgot-password" ~title:"Forgot Password" ~content ()

(* /reset-password through the same launch
   wrapper. Form contract unchanged — POST /reset-password, Dream CSRF tag,
   hidden 'token' field, 'password'/'confirm_password' names with their
   required+minlength flags, and the renderer-owned error alert keeps its
   legacy auth-alert classes and its position above the form. Passwords are
   never echoed back on re-render (unchanged). Fix carried by this pass: the
   hidden token value is now HTML-escaped — previously the raw ?token= query
   value (attacker-controlled) was interpolated verbatim into a single-quoted
   attribute, an injection vector on the validation re-render path. Error
   messages stay server-authored constants. Missing/invalid/expired-token
   branches still render through the legacy msg_page, untouched. *)
let reset_password_page ~token ?error request =
  let csrf_token = Csrf_field.tag request in
  let error_html =
    match error with
    | None -> Html.empty
    | Some msg ->
        Html.template "<div class='auth-alert auth-alert--error'>%s</div>"
          [ msg ]
  in
  let content =
    Html.template
      "\n\
      \        <div class='auth'>\n\
      \          <div class='auth__head'>\n\
      \            <img class='auth__mark' src='/static/images/logo-mark.svg' \
       alt=''>\n\
      \            <h1 class='auth__title'>Set new password</h1>\n\
      \            <p class='auth__sub'>Enter a new password for your \
       account.</p>\n\
      \          </div>\n\
      \          %s\n\
      \          <form action='/reset-password' method='POST' class='auth-form'>\n\
      \            %s\n\
      \            <input type='hidden' name='token' value='%s'>\n\
      \            <div class='auth-field'>\n\
      \                <label class='auth-label' for='rp-password'>New \
       password</label>\n\
      \                <input class='auth-input' type='password' \
       id='rp-password' name='password' required minlength='8'>\n\
      \            </div>\n\
      \            <div class='auth-field'>\n\
      \                <label class='auth-label' for='rp-confirm'>Confirm new \
       password</label>\n\
      \                <input class='auth-input' type='password' \
       id='rp-confirm' name='confirm_password' required minlength='8'>\n\
      \            </div>\n\
      \            <button type='submit' class='auth-btn'>Reset \
       password</button>\n\
      \          </form>\n\
      \          <div class='auth-foot'><a href='/login' \
       class='auth-link'>Back to login</a></div>\n\
      \        </div>"
      [ error_html; csrf_token; Html.text token ]
  in
  Page_shell.launch_auth_page ~noindex:true ~request
    ~page_class:"launch-reset-password" ~title:"Reset Password" ~content ()
