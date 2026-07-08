(** Brevo transactional email client.
    All functions are fire-and-forget: errors are logged, never re-raised,
    so a delivery failure never propagates into the HTTP response cycle. *)

(* Read-only: TRUE iff a non-empty BREVO_API_KEY is configured. Never exposes the key
   value — for the admin dashboard's safe configured/missing status only. *)
val is_configured : unit -> bool

val send_verification_email : to_email:string -> token:string -> unit Lwt.t
(* Pending-signup confirmation link (/confirm-email); clicking it creates the user. *)
val send_pending_signup_confirmation_email : to_email:string -> token:string -> unit Lwt.t
val send_password_reset_email : to_email:string -> token:string -> unit Lwt.t
