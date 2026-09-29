(** Brevo transactional email client.

    Best-effort: delivery failures are logged by message kind only (never
    the recipient, token or provider response) and never propagate into an
    HTTP response. The public signup and password-reset routes do not call
    the provider on the request path: they queue a {!message} on
    {!Auth_mail_dispatcher}, whose workers run {!deliver}. *)

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
    the promise is cancelable and cancellation closes the provider
    connection. [Error] is a short, secret-free failure class. *)

val deliver_via :
  endpoint:Uri.t -> api_key:string -> message -> (unit, string) result Lwt.t
(** [deliver] against an explicit endpoint — the production provider URL is
    fixed; this exists so the connection-release contract can be exercised
    against a local stalled server. *)

(** Awaited legacy entry points, bounded by the dispatcher's timeout. They
    never raise. The public routes use the dispatcher instead. *)

val send_verification_email : to_email:string -> token:string -> unit Lwt.t
val send_pending_signup_confirmation_email : to_email:string -> token:string -> unit Lwt.t
val send_password_reset_email : to_email:string -> token:string -> unit Lwt.t
