(* Brevo REST API client — fire-and-forget model: email failures are logged but
   never surface as user-visible errors. The verification token persists in the
   DB so the user can request a resend, removing a hard dependency on third-party
   delivery at signup time. *)

let brevo_api_url = "https://api.brevo.com/v3/smtp/email"

(* BASE_URL decouples the public hostname from HOST (which may be 0.0.0.0 for
   interface binding), so verification links work behind a reverse proxy in
   prod without changing the listener config. *)
let base_url () =
  Sys.getenv_opt "BASE_URL" |> Option.value ~default:"http://localhost:8080"

let sanitize_api_key s =
  let s = String.trim s in
  (* Strip surrounding double-quotes left by some .env parsers or bash exports. *)
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len - 1] = '"' then String.sub s 1 (len - 2)
  else s

(* Read-only status for the admin dashboard: TRUE iff a non-empty BREVO_API_KEY is
   present. Never returns or logs the key itself — only the boolean. A whitespace/quote-
   only value (a deploy mishap) reads as NOT configured, matching the send paths' own
   [Sys.getenv_opt] guard. Pure: alters no send behaviour. *)
let is_configured () =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | Some raw -> sanitize_api_key raw <> ""
  | None -> false

(* Token URLs are credential-equivalent: anyone reading the log can complete
   the verify/reset action. Off by default; opt-in via EARDE_LOG_TOKENS=1
   keeps local dev ergonomic without leaking tokens on shared/staging hosts. *)
let log_dev_token_url ~kind ~to_email ~url =
  match Sys.getenv_opt "EARDE_LOG_TOKENS" with
  | Some "1" ->
      Dream.log "BREVO_API_KEY not set — %s link for %s: %s" kind to_email url
  | _ ->
      Dream.log
        "BREVO_API_KEY not set — would send %s email to %s (set EARDE_LOG_TOKENS=1 to print link)"
        kind to_email

let send_verification_email ~to_email ~token =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      log_dev_token_url ~kind:"verification" ~to_email
        ~url:(Printf.sprintf "%s/verify?token=%s" (base_url ()) token);
      Lwt.return_unit
  | Some raw_key ->
      let api_key = sanitize_api_key raw_key in
      let verify_url = Printf.sprintf "%s/verify?token=%s" (base_url ()) token in
      let html_body =
        Printf.sprintf
          {|<html><body>
<p>Welcome to Earde!</p>
<p>Please verify your email address by clicking the link below:</p>
<p><a href="%s">Verify my account</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>If you did not create an account, you can safely ignore this email.</p>
</body></html>|}
          verify_url verify_url
      in
      let payload =
        Yojson.Safe.to_string
          (`Assoc
            [ ("sender", `Assoc [ ("name", `String "Earde"); ("email", `String "noreply@earde.com") ])
            ; ("to", `List [ `Assoc [ ("email", `String to_email) ] ])
            ; ("subject", `String "Verify your Earde account")
            ; ("htmlContent", `String html_body)
            ])
      in
      let headers =
        Cohttp.Header.of_list
          [ ("api-key", api_key)
          ; ("content-type", "application/json")
          ; ("accept", "application/json")
          ]
      in
      Lwt.catch
        (fun () ->
          let%lwt resp, body =
            Cohttp_lwt_unix.Client.post
              ~headers
              ~body:(Cohttp_lwt.Body.of_string payload)
              (Uri.of_string brevo_api_url)
          in
          let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
          (* Drain the body to release the connection but discard it: 4xx
             responses from upstreams sometimes echo request headers, and
             api-key in a log is the same blast radius as api-key in source. *)
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          if code >= 200 && code < 300 then
            Lwt.return_unit
          else begin
            Dream.log "Brevo API HTTP %d for %s" code to_email;
            Lwt.return_unit
          end)
        (fun exn ->
          Dream.log "Email delivery exception for %s: %s" to_email (Printexc.to_string exn);
          Lwt.return_unit)

(* Pending-signup confirmation. Distinct from send_verification_email (which targets the
   legacy /verify route on an existing users row): clicking THIS link is what creates the
   user, so it points at /confirm-email. Kept separate so legacy verification links and the
   pending-signup flow never share a URL. *)
let send_pending_signup_confirmation_email ~to_email ~token =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      log_dev_token_url ~kind:"confirmation" ~to_email
        ~url:(Printf.sprintf "%s/confirm-email?token=%s" (base_url ()) token);
      Lwt.return_unit
  | Some raw_key ->
      let api_key = sanitize_api_key raw_key in
      let confirm_url = Printf.sprintf "%s/confirm-email?token=%s" (base_url ()) token in
      let html_body =
        Printf.sprintf
          {|<html><body>
<p>Welcome to Earde!</p>
<p>Confirm your email address to finish creating your account:</p>
<p><a href="%s">Confirm my account</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>This link expires in 24 hours. If you did not sign up, you can safely ignore this email.</p>
</body></html>|}
          confirm_url confirm_url
      in
      let payload =
        Yojson.Safe.to_string
          (`Assoc
            [ ("sender", `Assoc [ ("name", `String "Earde"); ("email", `String "noreply@earde.com") ])
            ; ("to", `List [ `Assoc [ ("email", `String to_email) ] ])
            ; ("subject", `String "Confirm your Earde account")
            ; ("htmlContent", `String html_body)
            ])
      in
      let headers =
        Cohttp.Header.of_list
          [ ("api-key", api_key)
          ; ("content-type", "application/json")
          ; ("accept", "application/json")
          ]
      in
      Lwt.catch
        (fun () ->
          let%lwt resp, body =
            Cohttp_lwt_unix.Client.post
              ~headers
              ~body:(Cohttp_lwt.Body.of_string payload)
              (Uri.of_string brevo_api_url)
          in
          let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          if code >= 200 && code < 300 then
            Lwt.return_unit
          else begin
            Dream.log "Brevo API HTTP %d for %s" code to_email;
            Lwt.return_unit
          end)
        (fun exn ->
          Dream.log "Email delivery exception for %s: %s" to_email (Printexc.to_string exn);
          Lwt.return_unit)

let send_password_reset_email ~to_email ~token =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      log_dev_token_url ~kind:"password reset" ~to_email
        ~url:(Printf.sprintf "%s/reset-password?token=%s" (base_url ()) token);
      Lwt.return_unit
  | Some raw_key ->
      let api_key = sanitize_api_key raw_key in
      let reset_url = Printf.sprintf "%s/reset-password?token=%s" (base_url ()) token in
      let html_body =
        Printf.sprintf
          {|<html><body>
<p>You requested a password reset for your Earde account.</p>
<p>Click the link below to set a new password. This link expires in 2 hours.</p>
<p><a href="%s">Reset my password</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>If you did not request a password reset, you can safely ignore this email.</p>
</body></html>|}
          reset_url reset_url
      in
      let payload =
        Yojson.Safe.to_string
          (`Assoc
            [ ("sender", `Assoc [ ("name", `String "Earde"); ("email", `String "noreply@earde.com") ])
            ; ("to", `List [ `Assoc [ ("email", `String to_email) ] ])
            ; ("subject", `String "Reset your Earde password")
            ; ("htmlContent", `String html_body)
            ])
      in
      let headers =
        Cohttp.Header.of_list
          [ ("api-key", api_key)
          ; ("content-type", "application/json")
          ; ("accept", "application/json")
          ]
      in
      Lwt.catch
        (fun () ->
          let%lwt resp, body =
            Cohttp_lwt_unix.Client.post
              ~headers
              ~body:(Cohttp_lwt.Body.of_string payload)
              (Uri.of_string brevo_api_url)
          in
          let code = resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status in
          let%lwt () = Cohttp_lwt.Body.drain_body body in
          if code >= 200 && code < 300 then
            Lwt.return_unit
          else begin
            Dream.log "Brevo API HTTP %d for %s" code to_email;
            Lwt.return_unit
          end)
        (fun exn ->
          Dream.log "Email delivery exception for %s: %s" to_email (Printexc.to_string exn);
          Lwt.return_unit)
