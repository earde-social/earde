(** Signup, e-mail confirmation and verification, login, logout and password
    reset. Account-dependent answers are limited to what the account-privacy
    boundary allows (docs/features/account-privacy-boundaries.md). *)

val signups_enabled : unit -> bool

val is_valid_new_username : string -> bool
(** Route-safe ASCII syntax required of NEW usernames at signup: one or more of
    [A-Za-z], [0-9], ['_'], ['-'] — no quotes, angle brackets, slashes,
    whitespace or control characters. The existing 3..30 length bound is checked
    separately and still applies. Deliberately NOT enforced against accounts
    that already exist: login, lookup and rendering never consult it, so no
    current user is locked out or renamed. Escaping at each render sink remains
    the actual XSS defence; this is the second layer. Pure. *)

val signup_page : Dream.handler

val make_signup_handler :
  mail:Email.message Auth_mail_dispatcher.t -> Dream.handler
(** The signup POST over an explicit dispatcher. After the closed-signup,
    honeypot, Turnstile and syntax gates, the only account-dependent answer is
    "username taken" for a handle owned by a real account (decided by the
    username alone). Every other eligible submission is admitted to [mail] first
    (a full dispatcher answers a generic 503 before any hashing or write),
    always pays one Argon2 hash, and gets the same neutral response whether the
    email is new, registered or pending, whether a reservation is the
    submitter's own or someone else's, and whether a race or storage error
    prevented the write. A confirmation job is queued only after a fresh pending
    row commits. Every admitted submission, with or without a job, occupies one
    fixed-length service slot of [mail], so later admissions cannot observe
    which it was. *)

val signup_handler : Dream.handler
(** POST /signup on the process-wide auth mail dispatcher. *)

val verify_email_handler : Dream.handler
val confirm_email_handler : Dream.handler
val login_page : Dream.handler

val make_login_handler : verify:Login_verification.verifier -> Dream.handler
(** The login POST over an explicit verifier. A missing account and a wrong
    password get the same response after one verification each (the missing
    account against {!Login_verification.dummy_hash}); only a found account
    whose own hash verifies can log in. *)

val login_handler : Dream.handler
(** POST /login with the production Argon2 verifier. *)

val logout_handler : Dream.handler
val forgot_password_page : Dream.handler

val make_forgot_password_handler :
  mail:Email.message Auth_mail_dispatcher.t -> Dream.handler
(** The reset-request POST over an explicit dispatcher: admission first (a full
    dispatcher answers a generic 503 before any lookup or write), then the token
    write, then the same neutral response for every address. A reset job is
    queued only when a token row for a real account was written. Unknown
    addresses get no email, but occupy the same fixed service slot. *)

val forgot_password_handler : Dream.handler
(** POST /forgot-password on the process-wide auth mail dispatcher. *)

val reset_password_page_handler : Dream.handler
val reset_password_handler : Dream.handler
