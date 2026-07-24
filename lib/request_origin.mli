(** Same-origin browser check for state-changing POST endpoints, extracted
    verbatim from the GitHub installation-start handler so the project-setup
    POST can enforce the identical policy without a second copy of
    security-critical logic. Pure decision over the already-validated
    configuration and the request headers: no IO, no logging, and no header
    value is ever returned or reflected. *)

val same_origin_request : Github_app_config.t -> Dream.request -> bool
(** A present [Origin] header decides alone — it must match the validated
    public origin exactly (by normalized scheme, host, and effective port),
    and a malformed or mismatching value is rejected even if
    [Sec-Fetch-Site] claims same-origin. Only when [Origin] is absent does
    exactly ["same-origin"] fetch metadata suffice; ["same-site"],
    ["cross-site"], and ["none"] do not. Host, Referer, and forwarding
    headers are deliberately never consulted. *)
