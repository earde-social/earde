(** Brevo transactional email client.

    Best-effort: delivery never propagates into an HTTP response. The public
    signup and password-reset routes do not call the provider on the request
    path. They settle a {!message} into {!Auth_mail_dispatcher}, whose
    service slots run {!deliver}. Delivery-failure diagnostics name only the
    message kind and a failure class. The development path without
    BREVO_API_KEY is different: it logs the recipient, and with the explicit
    EARDE_LOG_TOKENS=1 opt-in it also logs the credential-bearing link. *)

(* Read-only: TRUE iff a non-empty BREVO_API_KEY is configured. Never exposes the key
   value — for the admin dashboard's safe configured/missing status only. *)
val is_configured : unit -> bool

type message
(** One transient authentication email. It carries the raw link token, so it
    is only ever held in memory — the database stores the token's hash. *)

val verification : to_email:string -> token:string -> message
(** Legacy [/verify] link for an existing users row. *)

val pending_signup_confirmation : to_email:string -> token:string -> message
(** [/confirm-email] link; clicking it is what creates the user. *)

val password_reset : to_email:string -> token:string -> message
(** [/reset-password] link. *)

val recipient : message -> string

val label : message -> string
(** A fixed, non-personal category for diagnostics. *)

val link : message -> string
(** The credential-bearing URL the email contains, built from BASE_URL. *)

val deliver : message -> (unit, string) result Lwt.t
(** One attempt through the configured provider (or the dev log line when
    BREVO_API_KEY is unset). Satisfies {!Auth_mail_dispatcher.transport}:
    the promise is cancelable, and cancelling it closes the provider
    connection, cancels a pending connect, or drops the wait on a pending
    name resolution. Resolution goes through a process-wide
    {!Auth_mail_resolver} with {!Auth_mail_resolver.default_permits}
    permits. When both are held by lookups still running (even abandoned
    ones), the attempt fails at once with ["resolver_busy"]. [Error] is a
    short, secret-free failure class. *)

val deliver_via :
  ?resolver:Cohttp_lwt_unix.Net.endp Auth_mail_resolver.t ->
  endpoint:Uri.t ->
  api_key:string ->
  message ->
  (unit, string) result Lwt.t
(** [deliver] against an explicit endpoint. The production provider URL is
    fixed. This exists so the connection-release and resolver-ownership
    contracts can be exercised against local servers. [resolver] defaults to
    the process-wide gate. *)
