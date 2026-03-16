(** Brevo transactional email client.
    All functions are fire-and-forget: errors are logged, never re-raised,
    so a delivery failure never propagates into the HTTP response cycle. *)

val send_verification_email : to_email:string -> token:string -> unit Lwt.t
val send_password_reset_email : to_email:string -> token:string -> unit Lwt.t
