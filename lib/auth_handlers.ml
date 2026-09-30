(* Public signup is gated so it can be reopened deliberately after the bot incident.
   Default CLOSED: only an explicit truthy EARDE_SIGNUPS_ENABLED opens it. *)
let signups_enabled () =
  match Sys.getenv_opt "EARDE_SIGNUPS_ENABLED" with
  | Some v ->
      (match String.lowercase_ascii (String.trim v) with
       | "1" | "true" | "yes" | "on" -> true
       | _ -> false)
  | None -> false

(* Route-safe ASCII syntax for NEW usernames. A username is a path segment
   (/u/:username), a form value, and a rendered identity on a public,
   crawlable profile; before this rule the only check was a length bound, so
   quotes and angle brackets could be registered and later rendered. Escaping
   at every sink is the actual XSS defence and stays in place — this is the
   second layer, and it also keeps handles unambiguous in URLs and @mentions
   (extract_mentions already recognises exactly this alphabet).

   Deliberately NOT applied to existing accounts: login, lookup and rendering
   never call it, so no current user is locked out or renamed. Pure, so it is
   unit-tested without a database. *)
let is_valid_new_username name =
  String.length name > 0
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '_' || c = '-')
       name

let signups_closed_page request =
  let user = Dream.session_field request "username" in
  Site_pages.msg_page ~auth:true ?user ~title:"Signups are closed"
    ~message:"Earde is in private alpha and public signups are temporarily closed. Check back soon."
    ~alert_type:"info" ~return_url:"/" request

(* Shown after a POST that produced (or would have produced) a pending signup. The honeypot
   path renders the SAME page so a bot can't tell from the response that it was caught, and
   so does every private signup outcome. Conditional wording: the email is queued after the
   response, so the page must not claim it was already sent. *)
let check_your_email_page request =
  let user = Dream.session_field request "username" in
  Site_pages.msg_page ~auth:true ?user ~title:"Check your email"
    ~message:"If everything checks out, we'll email you a confirmation link. Click it within 24 hours to finish creating your account. If nothing arrives, you can sign up again to get a new link."
    ~alert_type:"info" ~return_url:"/login" request

(* EARDE_TURNSTILE_REQUIRED is set but the Turnstile keys are missing/empty.
   Rather than serve a normal signup form that would create accounts with no bot
   protection, fail closed. Copy is generic so it doesn't reveal the misconfig. *)
let turnstile_unavailable_page request =
  let user = Dream.session_field request "username" in
  Site_pages.msg_page ~auth:true ?user ~title:"Signup temporarily unavailable"
    ~message:"Signups are temporarily unavailable. Please try again later."
    ~alert_type:"info" ~return_url:"/" request

let signup_page request =
  if not (signups_enabled ()) then Dream.html (signups_closed_page request)
  else
    let user = Dream.session_field request "username" in
    match Turnstile.status () with
    | Turnstile.Misconfigured -> Dream.html (turnstile_unavailable_page request)
    | Turnstile.Configured site_key ->
        Dream.html (Auth_pages.signup_form ?user ~turnstile_site_key:site_key request)
    | Turnstile.Disabled -> Dream.html (Auth_pages.signup_form ?user request)

(* Resolve the Turnstile outcome for a POST. Honeypot is handled by the caller
   FIRST (a caught bot never reaches here, so we spend no siteverify call on it).
   On `Failed the site key is returned so the form re-renders with a fresh widget. *)
let verify_turnstile form_data =
  match Turnstile.status () with
  | Turnstile.Misconfigured -> Lwt.return `Unavailable
  | Turnstile.Disabled -> Lwt.return (`Passed None)
  | Turnstile.Configured site_key ->
      let token =
        String.trim (List.assoc_opt "cf-turnstile-response" form_data |> Option.value ~default:"")
      in
      if token = "" then Lwt.return (`Failed site_key)
      else
        let%lwt ok = Turnstile.verify ~response:token in
        if ok then Lwt.return (`Passed (Some site_key)) else Lwt.return (`Failed site_key)

(* One bounded best-effort mail dispatcher for the public auth routes that
   send mail (signup confirmation and password reset). It is shared, so the
   64 outstanding / 2 slots / 15 s fixed-slot bounds cover both routes
   together, and a request's private outcome never changes how long it
   occupies them. *)
let auth_mail : Email.message Auth_mail_dispatcher.t =
  Auth_mail_dispatcher.create ~label:Email.label ~transport:Email.deliver ()

let username_unavailable_form ?turnstile_site_key request =
  let user = Dream.session_field request "username" in
  Dream.html (Auth_pages.signup_form ?user ?turnstile_site_key
                ~error:(Html.static "That username is already taken.") request)

let make_signup_handler ~mail request =
  (* Gate first: closed signup creates no pending row, no user, and sends no email. *)
  if not (signups_enabled ()) then Dream.html (signups_closed_page request)
  else
  match%lwt Dream.form request with
  | `Ok form_data ->
      let username = String.trim (List.assoc_opt "username" form_data |> Option.value ~default:"") in
      let email    = String.trim (List.assoc_opt "email"    form_data |> Option.value ~default:"") in
      let password = List.assoc_opt "password" form_data |> Option.value ~default:"" in
      (* Honeypot: a hidden field no human ever fills. If populated, silently drop —
         create nothing, send nothing — but show the normal "check your email" page. *)
      let honeypot = String.trim (List.assoc_opt "website" form_data |> Option.value ~default:"") in

      if honeypot <> "" then Dream.html (check_your_email_page request)
      else
      (* Turnstile gate: must pass BEFORE any precheck, argon2, pending row, or
         email. Fails closed — a missing/invalid token or a Cloudflare timeout
         creates nothing and sends nothing. *)
      (match%lwt verify_turnstile form_data with
      | `Unavailable -> Dream.html (turnstile_unavailable_page request)
      | `Failed site_key ->
          let user = Dream.session_field request "username" in
          Dream.html (Auth_pages.signup_form ?user ~turnstile_site_key:site_key
                        ~error:(Html.static "Human verification failed. Please try again.") request)
      | `Passed turnstile_site_key ->

      (* Validate before hashing — argon2 is expensive, reject obvious bad input early. *)
      if username = "" || email = "" || password = "" then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username, email, and password are all required." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length username < 3 || String.length username > 30 then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username must be between 3 and 30 characters." ~alert_type:"error" ~return_url:"/signup" request)
      else if not (is_valid_new_username username) then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Username can only contain letters, numbers, underscores and hyphens." ~alert_type:"error" ~return_url:"/signup" request)
      else if not (String.contains email '@') then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Please enter a valid email address." ~alert_type:"error" ~return_url:"/signup" request)
      else if String.length password < 8 then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Password must be at least 8 characters long." ~alert_type:"error" ~return_url:"/signup" request)
      else

      (* The one intentional public disclosure: a handle that belongs to a
         real account is reported as taken, from the username alone and
         before any hashing. Nothing about the EMAIL is looked at here. *)
      match%lwt Dream.sql request (fun db ->
        Signup_submission_store.username_registered db username) with
      | Error err ->
          Dream.html (Site_pages.msg_page ~auth:true ~title:"Registration Failed" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/signup" request)
      | Ok true -> username_unavailable_form ?turnstile_site_key request
      | Ok false ->

      (* From here on every private state — new, registered or pending email,
         own or foreign reservation, a lost uniqueness race, a storage error —
         pays the same admission, the same argon2 hash and one transaction,
         gets the same "check your email" answer, and occupies one fixed
         service slot whether or not it yields mail. Admission comes first
         so a full dispatcher refuses before anything is hashed or written. *)
      match%lwt Auth_mail_dispatcher.admit mail (fun () ->
        match%lwt Auth.hash_password password with
        | Error err -> Lwt.return (`Hash_failed err, None)
        | Ok password_hash ->
            let token = Dream.to_base64url (Dream.random 32) in
            let token_hash = Pending_signup_store.hash_token token in
            let ip = Some (Dream.client request) in
            let user_agent = Dream.header request "User-Agent" in
            (* No users row and no session are created here: only a confirmed
               (link-clicked) pending becomes a real user. The mail job is
               returned only after COMMIT, and the connection is back in the
               pool before any provider IO can start. *)
            let%lwt submitted = Dream.sql request (fun db ->
              let%lwt r =
                Signup_submission_store.submit db ~username ~email ~password_hash
                  ~token_hash ~ip ~user_agent
              in
              (* Best-effort secondary cleanup; correctness does not depend on it. *)
              let%lwt _ = Pending_signup_store.sweep_expired db in
              Lwt.return r)
            in
            (match submitted with
             | Ok Signup_submission_store.Pending_created ->
                 Lwt.return
                   (`Neutral, Some (Email.pending_signup_confirmation ~to_email:email ~token))
             | Ok Signup_submission_store.Username_taken ->
                 Lwt.return (`Username_taken, None)
             | Ok Signup_submission_store.Not_created -> Lwt.return (`Neutral, None)
             | Error err ->
                 (* Logged, then answered like every other private outcome: a
                    distinct error here could single out the branch that failed. *)
                 ignore (Handler_support.db_error_message err : string);
                 Lwt.return (`Neutral, None)))
      with
      | `Refused -> Rate_limit_middleware.temporarily_unavailable ~return_url:"/signup" request
      | `Admitted `Neutral -> Dream.html (check_your_email_page request)
      | `Admitted `Username_taken -> username_unavailable_form ?turnstile_site_key request
      | `Admitted (`Hash_failed err) ->
          Dream.html (Site_pages.msg_page ~auth:true ~title:"Security Error" ~message:("Security error: " ^ err) ~alert_type:"error" ~return_url:"/signup" request))

  | _ -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. The CSRF token was invalid or your session expired. Please try again." ~alert_type:"error" ~return_url:"/signup" request)

let signup_handler = make_signup_handler ~mail:auth_mail

let verify_email_handler request =
  match Dream.query request "token" with
  | None -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Verification Error" ~message:"The verification token is missing from the URL." ~alert_type:"error" ~return_url:"/signup" request)
  | Some token ->
      Dream.sql request (fun db ->
        match%lwt Credential_store.verify_email db token with
        | Ok (Some username) ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Email Verified!" ~message:(Printf.sprintf "Your account u/%s is now verified. You can log in." username) ~alert_type:"success" ~return_url:"/login" request)
        | Ok None ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Verification Failed" ~message:"This link is invalid or your email has already been verified." ~alert_type:"error" ~return_url:"/signup" request)
        | Error err -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request)
      )

let confirm_email_handler request =
  match Dream.query request "token" with
  | None ->
      Dream.html (Site_pages.msg_page ~auth:true ~title:"Confirmation Error" ~message:"The confirmation token is missing from the URL." ~alert_type:"error" ~return_url:"/signup" request)
  | Some token ->
      let token_hash = Pending_signup_store.hash_token token in
      let%lwt result =
        Dream.sql request (fun db -> Pending_signup_store.confirm db token_hash)
      in
      (match result with
        | Ok (`Confirmed (user_id, username, _email, created_at, is_admin)) ->
            (* The confirmation transaction returned the closed person
               properties with the insert, so the capture needs no lookup. A
               confirmation link opened without granted consent emits
               nothing. Email never enters the analytics payload. *)
            Analytics.capture_if_consented request
              ~distinct_id:(Analytics.distinct_id_of_user_id user_id)
              (Analytics.Account_signed_up
                 {
                   user_id;
                   person =
                     { Analytics.username; signup_date = created_at; is_admin };
                 });
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Email Confirmed!" ~message:(Printf.sprintf "Your account u/%s is now active. You can log in." username) ~alert_type:"success" ~return_url:"/login" request)
        | Ok `Invalid ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Confirmation Failed" ~message:"This confirmation link is invalid or has expired. Please sign up again." ~alert_type:"error" ~return_url:"/signup" request)
        | Ok `Conflict ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Already Registered" ~message:"An account with this username or email already exists. Please log in." ~alert_type:"error" ~return_url:"/login" request)
        | Error err ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/" request))

let login_page request =
  let user = Dream.session_field request "username" in
  Dream.html (Auth_pages.login_form ?user request)

let make_login_handler ~verify request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let identifier = List.assoc_opt "identifier" form_data |> Option.value ~default:"" in
      let password = List.assoc_opt "password" form_data |> Option.value ~default:"" in

      (* Credential check uses constant-message pattern: every failure path
         returns the same string to prevent username enumeration. Ban check
         happens only after password is verified to avoid leaking existence. *)
      let%lwt lookup =
        Dream.sql request (fun db -> User_store.get_user_for_login db identifier)
      in
      (match lookup with
        | Ok row ->
            (* Argon2 verification runs after the lookup's connection is back
               in the pool — CPU-bound work must not hold a connection open
               (same rule as reset_password_handler). A missing account is
               verified against the dummy hash, so both failure kinds cost
               one full verification. *)
            let candidate =
              Option.map
                (fun ((id, user, _email, created_at), (hash, is_admin, is_banned)) ->
                  (hash, (id, user, created_at, is_admin, is_banned)))
                row
            in
            (match%lwt Login_verification.authenticate ~verify ~password candidate with
            | Some (id, user, created_at, is_admin, is_banned) ->
                if is_banned then
                  Dream.html (Site_pages.msg_page ~auth:true ~title:"Account Banned" ~message:"Your account has been permanently banned from Earde." ~alert_type:"error" ~return_url:"/login" request)
                else
                  (* The browser may present a session that already carries
                     another user's identity (or a pre-auth session an attacker
                     could have fixated). Invalidate it so the new login starts
                     from a fresh, empty session with a rotated session id,
                     then write every canonical auth field from the newly
                     authenticated row — is_admin unconditionally, so a prior
                     admin session can never leak privileges into this one. *)
                  let%lwt () = Dream.invalidate_session request in
                  let%lwt () = Dream.set_session_field request "user_id" (string_of_int id) in
                  let%lwt () = Dream.set_session_field request "username" user in
                  let%lwt () = Dream.set_session_field request "is_admin" (if is_admin then "true" else "false") in
                  (* Exactly once per fully successful login (credentials
                     verified, not banned); the incoming request still carries
                     the consent cookie the gate reads. *)
                  Analytics.capture_if_consented request
                    ~distinct_id:(Analytics.distinct_id_of_user_id id)
                    (Analytics.Account_logged_in
                       {
                         user_id = id;
                         person =
                           { Analytics.username = user; signup_date = created_at; is_admin };
                       });
                  Dream.redirect request "/"
            | None -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Login Failed" ~message:"Invalid username or password." ~alert_type:"error" ~return_url:"/login" request))
        | Error err -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:(Handler_support.db_error_message err) ~alert_type:"error" ~return_url:"/login" request))
  | _ -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Form Error" ~message:"There was a problem with your form submission. Your session may have expired." ~alert_type:"error" ~return_url:"/login" request)

let login_handler = make_login_handler ~verify:Login_verification.argon2_verifier

let logout_handler request =
  let%lwt () = Dream.invalidate_session request in
  Dream.redirect request "/"

let forgot_password_page request = Dream.html (Auth_pages.forgot_password_page request)

(* Never confirm or deny email existence: every account state gets the same
   response. The reset email is never awaited, and every admitted request
   occupies one fixed service slot whether or not it yields mail, so the
   shared capacity does not reveal the outcome either. Admission happens
   before the lookup and the token write, so a full dispatcher refuses
   identically for all addresses and writes nothing. *)
let make_forgot_password_handler ~mail request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let email = String.trim (List.assoc_opt "email" form_data |> Option.value ~default:"") in
      if email = "" then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Validation Error" ~message:"Email address is required." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else
        (match%lwt Auth_mail_dispatcher.admit mail (fun () ->
          let token = Dream.to_base64url (Dream.random 32) in
          (* The token row is committed (a single INSERT ... SELECT) before
             the job is returned, and the connection is released before any
             provider IO. No row, no mail: a nonexistent account gets no
             dummy email. *)
          let%lwt result = Dream.sql request (fun db ->
            Credential_store.create_token db email token
          ) in
          match result with
          | Ok true -> Lwt.return ((), Some (Email.password_reset ~to_email:email ~token))
          | Ok false -> Lwt.return ((), None)
          | Error err ->
              Dream.log "forgot_password DB error: %s" err;
              Lwt.return ((), None))
        with
        | `Refused -> Rate_limit_middleware.temporarily_unavailable ~return_url:"/forgot-password" request
        | `Admitted () ->
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Check your email" ~message:"If an account with that email exists, we'll email a reset link to it shortly. Check your inbox (and spam folder); if nothing arrives, you can request a new link." ~alert_type:"info" ~return_url:"/login" request))
  | _ -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)

let forgot_password_handler = make_forgot_password_handler ~mail:auth_mail

let reset_password_page_handler request =
  match Dream.query request "token" with
  | None ->
      Dream.html (Site_pages.msg_page ~auth:true ~title:"Invalid Link" ~message:"This password reset link is missing a token. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
  | Some token ->
      (match%lwt Dream.sql request (fun db -> Credential_store.validate_token db token) with
      | Ok (Some _) -> Dream.html (Auth_pages.reset_password_page ~token request)
      | Ok None ->
          Dream.html (Site_pages.msg_page ~auth:true ~title:"Link Expired" ~message:"This password reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
      | Error _ ->
          Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request))

(* Argon2 hashing runs before the DB transaction — CPU-bound work must not hold
   a connection open. The token DELETE and password UPDATE are atomic: a hash or
   DB error rolls back the DELETE so the user keeps the reset link. *)
let reset_password_handler request =
  match%lwt Dream.form request with
  | `Ok form_data ->
      let token    = List.assoc_opt "token"            form_data |> Option.value ~default:"" in
      let password = List.assoc_opt "password"         form_data |> Option.value ~default:"" in
      let confirm  = List.assoc_opt "confirm_password" form_data |> Option.value ~default:"" in
      if token = "" then
        Dream.html (Site_pages.msg_page ~auth:true ~title:"Invalid Request" ~message:"Token is missing. Please use the link from your email." ~alert_type:"error" ~return_url:"/forgot-password" request)
      else if password <> confirm then
        Dream.html (Auth_pages.reset_password_page ~token ~error:(Html.static "Passwords do not match.") request)
      else if String.length password < 8 then
        Dream.html (Auth_pages.reset_password_page ~token ~error:(Html.static "Password must be at least 8 characters.") request)
      else
        (match%lwt Auth.hash_password password with
        | Error err ->
            Dream.log "reset_password hash error: %s" err;
            Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)
        | Ok new_hash ->
            Dream.sql request (fun db ->
              match%lwt Credential_store.reset_password_atomically db token new_hash with
              | Ok false ->
                  Dream.html (Site_pages.msg_page ~auth:true ~title:"Link Expired" ~message:"This reset link is invalid or has expired. Please request a new one." ~alert_type:"error" ~return_url:"/forgot-password" request)
              | Ok true ->
                  Dream.html (Site_pages.msg_page ~auth:true ~title:"Password Updated" ~message:"Your password has been updated. You can now log in with your new password." ~alert_type:"success" ~return_url:"/login" request)
              | Error err ->
                  Dream.log "reset_password error: %s" err;
                  Dream.html (Site_pages.msg_page ~auth:true ~title:"Error" ~message:"An error occurred. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)))
  | _ -> Dream.html (Site_pages.msg_page ~auth:true ~title:"Form Error" ~message:"Your form submission failed. Please try again." ~alert_type:"error" ~return_url:"/forgot-password" request)
