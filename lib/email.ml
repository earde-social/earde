(* Brevo REST API client — best-effort model: email failures are logged but
   never surface as user-visible errors. The verification token persists in
   the DB (as a hash) so the user can request a resend, removing a hard
   dependency on third-party delivery at signup time. The public signup and
   password-reset routes never await this client: they settle a [message]
   into Auth_mail_dispatcher, which starts [deliver] when the message's
   service slot begins. That can be before the HTTP response is written. *)

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

type kind = Verification | Signup_confirmation | Password_reset

(* Holds the raw token only for as long as the message is in memory: it is
   the one credential-bearing payload, never persisted and never logged
   (outside the explicit EARDE_LOG_TOKENS dev opt-in below). *)
type message = { kind : kind; to_email : string; token : string }

let verification ~to_email ~token = { kind = Verification; to_email; token }

let pending_signup_confirmation ~to_email ~token =
  { kind = Signup_confirmation; to_email; token }

let password_reset ~to_email ~token = { kind = Password_reset; to_email; token }
let recipient m = m.to_email

(* A fixed category per kind: the only message detail a diagnostic may name. *)
let label m =
  match m.kind with
  | Verification -> "verification"
  | Signup_confirmation -> "signup_confirmation"
  | Password_reset -> "password_reset"

(* Distinct paths per kind: the legacy /verify link targets an existing users
   row, while clicking /confirm-email is what creates the user from a pending
   signup, so the two flows never share a URL. *)
let link m =
  let path =
    match m.kind with
    | Verification -> "/verify"
    | Signup_confirmation -> "/confirm-email"
    | Password_reset -> "/reset-password"
  in
  Printf.sprintf "%s%s?token=%s" (base_url ()) path m.token

let subject m =
  match m.kind with
  | Verification -> "Verify your Earde account"
  | Signup_confirmation -> "Confirm your Earde account"
  | Password_reset -> "Reset your Earde password"

let html_body m =
  let url = link m in
  match m.kind with
  | Verification ->
      Html.template
        {|<html><body>
<p>Welcome to Earde!</p>
<p>Please verify your email address by clicking the link below:</p>
<p><a href="%s">Verify my account</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>If you did not create an account, you can safely ignore this email.</p>
</body></html>|}
        [ Html.external_url url; Html.text url ]
  | Signup_confirmation ->
      Html.template
        {|<html><body>
<p>Welcome to Earde!</p>
<p>Confirm your email address to finish creating your account:</p>
<p><a href="%s">Confirm my account</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>This link expires in 24 hours. If you did not sign up, you can safely ignore this email.</p>
</body></html>|}
        [ Html.external_url url; Html.text url ]
  | Password_reset ->
      Html.template
        {|<html><body>
<p>You requested a password reset for your Earde account.</p>
<p>Click the link below to set a new password. This link expires in 2 hours.</p>
<p><a href="%s">Reset my password</a></p>
<p>Or copy this URL into your browser:<br>%s</p>
<p>If you did not request a password reset, you can safely ignore this email.</p>
</body></html>|}
        [ Html.external_url url; Html.text url ]

let payload m =
  Yojson.Safe.to_string
    (`Assoc
       [
         ( "sender",
           `Assoc
             [
               ("name", `String "Earde"); ("email", `String "noreply@earde.com");
             ] );
         ("to", `List [ `Assoc [ ("email", `String m.to_email) ] ]);
         ("subject", `String (subject m));
         ("htmlContent", `String (Html.to_string (html_body m)));
       ])

let dev_kind_name m =
  match m.kind with
  | Verification -> "verification"
  | Signup_confirmation -> "confirmation"
  | Password_reset -> "password reset"

(* Token URLs are credential-equivalent: anyone reading the log can complete
   the verify/reset action. Off by default; opt-in via EARDE_LOG_TOKENS=1
   keeps local dev ergonomic without leaking tokens on shared/staging hosts. *)
let log_dev_token_url m =
  match Sys.getenv_opt "EARDE_LOG_TOKENS" with
  | Some "1" ->
      Dream.log "BREVO_API_KEY not set — %s link for %s: %s" (dev_kind_name m)
        m.to_email (link m)
  | _ ->
      Dream.log
        "BREVO_API_KEY not set — would send %s email to %s (set \
         EARDE_LOG_TOKENS=1 to print link)"
        (dev_kind_name m) m.to_email

module Connection = Cohttp_lwt_unix.Connection

(* The one resolver gate for authentication mail in this process. The
   physical lookup is cohttp's own (conduit's system resolver: getservbyname
   and getaddrinfo jobs in Lwt's thread pool), unchanged, so the endpoint,
   TLS and SNI behaviour is exactly what Client.post used. *)
let auth_mail_resolver : Cohttp_lwt_unix.Net.endp Auth_mail_resolver.t =
  Auth_mail_resolver.create ~permits:Auth_mail_resolver.default_permits
    ~resolve:(fun uri ->
      Cohttp_lwt_unix.Net.resolve
        ~ctx:(Lazy.force Cohttp_lwt_unix.Net.default_ctx)
        uri)

(* One POST on a connection this function owns, so that giving up on it
   really releases it. Cohttp's plain Client.post closes its connection only
   once a response body has been consumed: a provider that accepts the
   request and never answers would keep the socket (and its reader/writer
   loops) alive after the caller stopped waiting. Here the returned promise
   is a cancelable task. Cancelling it drops the wait on a pending name
   resolution, cancels a pending connect (conduit closes the half-open
   socket) or closes the established connection. Every settled outcome
   closes the connection too.

   Name resolution cannot be interrupted, so it goes through
   [auth_mail_resolver]: at most two lookups exist at once, counting the
   ones still running for attempts that were already abandoned. When both
   permits are held, the attempt fails at once ("resolver_busy") instead of
   starting or queueing another lookup. An abandoned attempt drops its own
   continuation, so the lookup that outlives it holds no payload, recipient,
   token or key, and it never goes on to connect.

   Connecting is awaited before the request is queued, so a refused or
   unreachable provider fails at once. *)
let post_owned ~resolver ~endpoint ~api_key body_string =
  let result, resolver_u = Lwt.task () in
  let abandoned = ref false in
  let resolving = ref None in
  let connecting = ref None in
  let connection = ref None in
  let release () =
    abandoned := true;
    (match !resolving with
    | Some p ->
        resolving := None;
        Lwt.cancel p
    | None -> ());
    (match !connecting with
    | Some p ->
        connecting := None;
        Lwt.cancel p
    | None -> ());
    match !connection with
    | Some c ->
        connection := None;
        Connection.close c
    | None -> ()
  in
  let finish outcome =
    release ();
    if Lwt.is_sleeping result then Lwt.wakeup_later resolver_u outcome
  in
  Lwt.on_cancel result release;
  let headers =
    Cohttp.Header.of_list
      [
        ("api-key", api_key);
        ("content-type", "application/json");
        ("accept", "application/json");
      ]
  in
  Lwt.dont_wait
    (fun () ->
      let ctx = Lazy.force Cohttp_lwt_unix.Net.default_ctx in
      let resolution = Auth_mail_resolver.resolve resolver endpoint in
      resolving := Some resolution;
      let%lwt resolved = resolution in
      resolving := None;
      match resolved with
      | Error `Busy ->
          finish (Error "resolver_busy");
          Lwt.return_unit
      | Ok _ when !abandoned -> Lwt.return_unit
      | Ok endp ->
          let pending = Connection.connect ~persistent:true ~ctx endp in
          connecting := Some pending;
          let%lwt c = pending in
          connecting := None;
          if !abandoned then begin
            Connection.close c;
            Lwt.return_unit
          end
          else begin
            connection := Some c;
            let%lwt resp, body =
              Connection.call c ~headers
                ~body:(Cohttp_lwt.Body.of_string body_string)
                `POST endpoint
            in
            let code =
              resp |> Cohttp.Response.status |> Cohttp.Code.code_of_status
            in
            (* Drain the body but discard it: 4xx responses from upstreams
               sometimes echo request headers, and api-key in a log is the same
               blast radius as api-key in source. *)
            let%lwt () = Cohttp_lwt.Body.drain_body body in
            finish
              (if code >= 200 && code < 300 then Ok ()
               else Error (Printf.sprintf "http_%d" code));
            Lwt.return_unit
          end)
    (fun _exn -> finish (Error "transport_error"));
  result

let deliver_via ?(resolver = auth_mail_resolver) ~endpoint ~api_key m =
  post_owned ~resolver ~endpoint ~api_key (payload m)

let deliver m =
  match Sys.getenv_opt "BREVO_API_KEY" with
  | None ->
      log_dev_token_url m;
      Lwt.return (Ok ())
  | Some raw_key ->
      deliver_via
        ~endpoint:(Uri.of_string brevo_api_url)
        ~api_key:(sanitize_api_key raw_key) m
