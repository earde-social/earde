(* DREAM_SECRET env var required in production (256-bit key via
   `Dream.to_base64url (Dream.random 32)`). Falls back to ephemeral
   random secret in dev — sessions won't survive restarts. *)
let () =
  (* Fail fast on missing DATABASE_URL: a hardcoded fallback would either ship
     real credentials in source or train operators to ignore the env contract. *)
  let db_url =
    match Sys.getenv_opt "DATABASE_URL" with
    | Some url -> url
    | None ->
        prerr_endline "FATAL: DATABASE_URL environment variable is required.";
        exit 1
  in
  (* Pool sized for launch-day surges; tunable per-deploy without recompile.
     Hard-fail on garbage input rather than silently falling back — a typo
     here would quietly cap us at 32 under load and be invisible in logs. *)
  let db_pool_size =
    match Sys.getenv_opt "DB_POOL_SIZE" with
    | None -> 32
    | Some s -> (
        match int_of_string_opt (String.trim s) with
        | Some n when n > 0 -> n
        | _ ->
            prerr_endline "FATAL: DB_POOL_SIZE must be a positive integer.";
            exit 1)
  in
  let secret_middleware =
    match Sys.getenv_opt "DREAM_SECRET" with
    | Some s -> Dream.set_secret s
    | None -> Fun.id
  in
  let interface =
    match Sys.getenv_opt "HOST" with Some h -> h | None -> "localhost"
  in
  (* Dream alpha7 has no Dream.proxy, so the forwarded-header trust boundary
     is ours to draw. It is drawn once, here, and every consumer of
     Dream.client (the rate limiter, the stored signup IP, the admin view)
     inherits it.

     This used to take the LEFTMOST X-Forwarded-For value unconditionally.
     That value is the one segment of the header a client fully controls, so
     rotating it minted an unlimited supply of rate-limit buckets; and with
     no header at all Dream.client kept its "address:port" form, whose
     ephemeral port minted a fresh bucket per connection. Client_address
     fixes both: forwarded headers count only when the immediate peer is a
     configured trusted proxy, only the rightmost entry (the address that
     proxy itself observed) is believed, and the result is always a bare
     normalized address.

     Resolved once per process — the trusted set is deployment topology, not
     per-request state. *)
  let trusted_proxies = Earde.Client_address.trusted_proxies_from_env () in
  let proxy handler request =
    Dream.set_client request
      (Earde.Client_address.client_ip ~trusted_proxies
         ~peer:(Dream.client request)
         ~forwarded_for:
           (* The last header line, as Client_address expects: a proxy that
              adds its own line instead of appending to the client's leaves
              the client's line first. *)
           (match List.rev (Dream.headers request "X-Forwarded-For") with
           | last :: _ -> Some last
           | [] -> None));
    handler request
  in
  Dream.run ~interface ~port:8080
  @@ proxy
  (* Replaces sensitive query parameter values (token=, state=, code=) with
     [REDACTED] before Dream.logger and analytics_middleware read the target,
     so those secrets never appear in access logs or page_views. The original
     target is stashed in a field private to Request_target_redaction and put
     back by its restore_middleware below, just before the router — without
     that, [REDACTED] would reach Dream.query and silently break every
     sensitive-parameter GET route (/verify, /reset-password, /confirm-email). *)
  @@ Earde.Request_target_redaction.redact_middleware
  @@ Dream.logger
  @@ Dream.sql_pool ~size:db_pool_size db_url
  @@ secret_middleware
  (* OUTSIDE sql_sessions so it sees the session's own Set-Cookie. Dream
     infers Secure from its TLS listener flag, which is false behind a
     TLS-terminating nginx, and neither sql_sessions nor any exported setter
     can override it — so the attribute is added to the outgoing header
     instead, decided by EARDE_PUBLIC_ORIGIN (server-side configuration, not
     a forwarded header). No-op on an http origin, so local development is
     unchanged. See session_cookie_policy.mli. *)
  @@ Earde.Session_cookie_policy.middleware
  (* sql_sessions trades ~1ms per-request DB round-trip for crash-safe session
     persistence. memory_sessions is zero-latency but loses all sessions on
     every systemd restart, forcing mass re-login. *)
  @@ Dream.sql_sessions
  (* Inside sql_pool + sql_sessions: needs Dream.sql and the session's user_id.
     Separate from analytics_middleware so removing page-view analytics later
     cannot take last_active_at (moderator auto-demotion input) down with it. *)
  @@ Earde.Activity_middleware.presence_middleware
  @@ Earde.Activity_middleware.analytics_middleware
  (* Inside sql_pool + sql_sessions, like presence: resolves the signed-in
     user's unread-notification count once and stashes it on the request, so
     every authenticated document renders its top-bar badge from the same
     durable query instead of each page fetching its own answer. Best-effort
     — a failed count leaves the field unset and the badge simply absent. *)
  @@ Earde.Notification_badge.middleware
  (* Runs AFTER Dream.logger and analytics_middleware (both must see the
     redacted target) but BEFORE the router, so only the route handler gets
     the real sensitive query parameters back via Dream.query. No-op for
     requests that had nothing to redact. *)
  @@ Earde.Request_target_redaction.restore_middleware
  @@ Earde.App_routes.router
