(** Signup, login and password-reset pages. *)

val signup_form : ?user:string -> ?error:string -> ?turnstile_site_key:string -> Dream.request -> string

val login_form : ?user:string -> Dream.request -> string

val forgot_password_page : Dream.request -> string

val reset_password_page : token:string -> ?error:string -> Dream.request -> string
