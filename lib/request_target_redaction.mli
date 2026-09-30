(** Request-target redaction for sensitive query parameters.

    Access logs, analytics, and the rate limiter must never see secret query
    values (password-reset/verification [token], GitHub onboarding CSRF [state],
    OAuth authorization [code]). This module owns the pure redaction, the
    path-only extraction used for non-secret request classification, and the
    Dream middleware pair that swaps the request target for a redacted one
    during logging/analytics and restores the original just before routing.

    The Dream field holding the original target is private to this module:
    handlers get the original values back only through the restored target. *)

val redact_target : string -> string
(** Replaces the value of every [token], [state], or [code] query parameter with
    [[REDACTED]]. Key matching is exact and case-sensitive, and only at a
    query-component boundary (immediately after [?] or [&], followed by [=] or
    the end of the component). Everything else — path, other parameters,
    ordering, percent encoding, any fragment-like suffix — is preserved
    byte-for-byte, and the input is returned unchanged when no sensitive
    parameter is present. Never raises. *)

val path_only : string -> string
(** Path component of a request target, with the entire query and any
    fragment-like suffix removed. Returns ["/"] for an empty or unusable path.
    Suitable for rate-limit keys and other non-secret classification: no query
    value, redacted or otherwise, ever appears in the result. *)

val redact_middleware : Dream.middleware
(** Rewrites the request target to [redact_target]'s output before invoking the
    rest of the pipeline, stashing the exact original target in a private field
    when (and only when) redaction changed it. Install before [Dream.logger] and
    analytics. *)

val restore_middleware : Dream.middleware
(** Restores the exact original target stashed by [redact_middleware], or does
    nothing if no redaction happened. Install after logging/analytics and
    immediately before [Dream.router], so route handlers read the real values.
*)
