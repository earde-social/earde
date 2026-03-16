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

let send_verification_email ~to_email ~token =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      (* Dev fallback: log the link so manual testing is still possible. *)
      Dream.log "BREVO_API_KEY not set — verification link for %s: %s/verify?token=%s"
        to_email (base_url ()) token;
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
          let%lwt body_str = Cohttp_lwt.Body.to_string body in
          if code >= 200 && code < 300 then
            Lwt.return_unit
          else begin
            Dream.log "Brevo API HTTP %d for %s — body: %s" code to_email body_str;
            Lwt.return_unit
          end)
        (fun exn ->
          Dream.log "Email delivery exception for %s: %s" to_email (Printexc.to_string exn);
          Lwt.return_unit)

let send_password_reset_email ~to_email ~token =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      Dream.log "BREVO_API_KEY not set — password reset link for %s: %s/reset-password?token=%s"
        to_email (base_url ()) token;
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
          let%lwt body_str = Cohttp_lwt.Body.to_string body in
          if code >= 200 && code < 300 then
            Lwt.return_unit
          else begin
            Dream.log "Brevo API HTTP %d for %s — body: %s" code to_email body_str;
            Lwt.return_unit
          end)
        (fun exn ->
          Dream.log "Email delivery exception for %s: %s" to_email (Printexc.to_string exn);
          Lwt.return_unit)
