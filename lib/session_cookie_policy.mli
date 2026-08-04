(** [Secure] on the session cookie behind a TLS-terminating proxy.

    {2 Why this is a response filter and not a configuration}

    Dream infers a cookie's [Secure] attribute (and its [__Host-]/[__Secure-]
    prefix) from [Helpers.tls request], which is set by Dream's own listener.
    Behind nginx, Dream speaks plain HTTP on loopback and that flag is false,
    so the session cookie ships without [Secure]. Three routes out of this
    exist in principle and none is open:

    - [Dream.sql_sessions] takes [?lifetime] and nothing else — there is no
      hook for cookie attributes;
    - Dream's session back end calls [Cookie.set_cookie] internally with
      neither [~secure] nor [~prefix], so both are always inferred;
    - [Dream.tls] is exported as a getter, but [set_tls] is not exported at
      all, so an application cannot tell Dream the request arrived over TLS.

    The application's own cookies already do this correctly and explicitly
    ([Github_onboarding_cookie], the analytics consent cookie): only the
    cookie Dream sets for itself is out of reach. This module therefore adds
    the attribute to the outgoing header, which is the one surface Dream does
    expose.

    {2 What it does and does not change}

    The decision comes from [EARDE_PUBLIC_ORIGIN] — server-side deployment
    configuration, the same source the consent cookie and the GitHub
    onboarding cookie use — and never from [X-Forwarded-Proto] or any other
    client-supplied header. Local development on [http://] is unaffected, so
    a plain-HTTP dev server keeps working.

    Only the [Secure] attribute is added. The cookie NAME is deliberately
    untouched: Dream computes the [__Host-] prefix from the same [tls] flag
    when it READS the cookie back, so renaming it in the response would make
    Dream unable to find it — session rotation and deletion would both break.
    [HttpOnly], [SameSite], [Path] and [Max-Age] are left exactly as emitted. *)

val secure_required : string option -> bool
(** Whether cookies must carry [Secure], given the configured public origin.
    True only for an [https://] origin. Pure. *)

val has_secure_attribute : string -> bool
(** Whether a [Set-Cookie] header value already carries a [Secure] attribute.
    Attribute names are case-insensitive, and the check is anchored to
    [';']-separated attributes so a cookie whose NAME or VALUE merely
    contains the word is not mistaken for one that is already secure. *)

val add_secure_attribute : string -> string
(** [Set-Cookie] value with [; Secure] appended, or unchanged when it is
    already present. Pure. *)

val middleware : Dream.middleware
(** Appends [Secure] to every [Set-Cookie] header of the response when the
    configured public origin is https. A no-op otherwise, and a no-op for
    responses that set no cookie. Place it OUTSIDE the session middleware so
    it sees the session's own [Set-Cookie]. *)
