let signup_form ?user:_ ?error ?turnstile_site_key request =
  let csrf_token = Csrf_field.tag request in
  (* Server-authored messages only (never user input); rendered as the flat
     rejected notice above the card — no toast, no animation. *)
  let error_html = match error with
    | None -> Html.empty
    | Some msg -> (Html.template "<p class='notice notice--rejected launch-auth__alert'>%s</p>"
  [ msg ])
  in
  (* Turnstile widget is rendered only when a site key is configured. The site key
     is public; still escape it as defense-in-depth. The challenge needs JS to
     solve, but the surrounding SSR form is unaffected when JS is off. *)
  let turnstile_widget, turnstile_script = match turnstile_site_key with
    | None -> Html.empty, Html.empty
    | Some key ->
        (Html.template "<div class='cf-turnstile' data-sitekey='%s'></div>"
  [ (Html.text (key)) ]),
        (Html.static "<script src='https://challenges.cloudflare.com/turnstile/v0/api.js' async defer></script>")
  in
  let content = (Html.template "
        <div class='auth'>
          <div class='auth__head'>
            <img class='auth__mark' src='/static/images/logo-mark.svg' alt=''>
            <h1 class='auth__title'>Create an account</h1>
            <p class='auth__sub'>One account for every community on Earde.</p>
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
                <span>I have read and accept the <a href='/privacy' target='_blank'>Privacy Policy</a>.</span>
            </label>

            %s

            <button type='submit' class='btn btn--primary btn--block launch-auth__submit'>Create account</button>
          </form>
          %s
          <p class='notice launch-auth__notice'>Maintainers: create your account first, then <a href='/bring'>connect a project through GitHub</a>.</p>
          <p class='auth__foot'>Already have an account? <a href='/login'>Log in &#8594;</a></p>
        </div>"
  [ error_html
  ; csrf_token
  ; turnstile_widget
  ; turnstile_script ])
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
  let content = (Html.template "
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
          <p class='notice launch-auth__notice'>Maintaining an open-source project? <a href='/bring'>Connect it through GitHub</a> after logging in.</p>
          <p class='auth__foot'>No account? <a href='/signup'>Create one &#8594;</a></p>
        </div>"
  [ csrf_token ])
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
  let content = (Html.template "
        <div class='auth'>
          <div class='auth__head'>
            <img class='auth__mark' src='/static/images/logo-mark.svg' alt=''>
            <h1 class='auth__title'>Forgot password?</h1>
            <p class='auth__sub'>Enter your email address and we'll send you a reset link.</p>
          </div>
          <form action='/forgot-password' method='POST' class='auth-form'>
            %s
            <div class='auth-field'>
                <label class='auth-label' for='fp-email'>Email address</label>
                <input class='auth-input' type='email' id='fp-email' name='email' required placeholder='you@example.com'>
            </div>
            <button type='submit' class='auth-btn'>Send reset link</button>
          </form>
          <div class='auth-foot'><a href='/login' class='auth-link'>Back to login</a></div>
        </div>"
  [ csrf_token ])
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
  let error_html = match error with
    | None -> Html.empty
    | Some msg -> (Html.template "<div class='auth-alert auth-alert--error'>%s</div>"
  [ msg ])
  in
  let content = (Html.template "
        <div class='auth'>
          <div class='auth__head'>
            <img class='auth__mark' src='/static/images/logo-mark.svg' alt=''>
            <h1 class='auth__title'>Set new password</h1>
            <p class='auth__sub'>Enter a new password for your account.</p>
          </div>
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
          </form>
          <div class='auth-foot'><a href='/login' class='auth-link'>Back to login</a></div>
        </div>"
  [ error_html
  ; csrf_token
  ; (Html.text (token)) ])
  in
  Page_shell.launch_auth_page ~noindex:true ~request
    ~page_class:"launch-reset-password" ~title:"Reset Password" ~content ()
