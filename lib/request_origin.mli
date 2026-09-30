(** Same-origin browser check for state-changing POST endpoints, extracted
    verbatim from the GitHub installation-start handler so the project-setup
    POST can enforce the identical policy without a second copy of
    security-critical logic. Pure decision over the already-validated
    configuration and the request headers: no IO, no logging, and no header
    value is ever returned or reflected. *)

val referrer_policy : string
(** The [Referrer-Policy] value every HTML page hosting a form that posts to a
    {!same_origin_request}-gated route must be served with. A document served
    ["no-referrer"] makes browsers attach [Origin: null] to the form POST it
    starts (Fetch, "append a request [Origin] header"), which this module
    rejects; ["same-origin"] keeps the real origin on that POST while still
    suppressing the [Referer] entirely for every cross-origin destination.
    Redirects away from state-bearing callback URLs keep their own
    ["no-referrer"] — they render no form. *)

val same_origin_request : Github_app_config.t -> Dream.request -> bool
(** A present [Origin] header decides alone — it must match the validated public
    origin exactly (by normalized scheme, host, and effective port), and a
    malformed or mismatching value is rejected even if [Sec-Fetch-Site] claims
    same-origin. Only when [Origin] is absent does exactly ["same-origin"] fetch
    metadata suffice; ["same-site"], ["cross-site"], and ["none"] do not. Host,
    Referer, and forwarding headers are deliberately never consulted. *)
