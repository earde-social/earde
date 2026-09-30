# Running Earde in production

This is a generic outline, not a turnkey recipe. It describes what a deployment
needs; adapt it to your own hosts and tooling.

## Components

- **Application:** `earde_server` (`dune build`, then run `_build/default/bin/main.exe`)
  - listens on port 8080, on `HOST` (default `localhost`);
  - serves `static/` from its working directory.
- **PostgreSQL 16**, migrated with `dbmate up`.
- **Realtime gateway** (optional): `gleam run` in `services/realtime_gateway`.
  It listens on 127.0.0.1:8090.
- **Reverse proxy** in front of both, terminating TLS.

## Configuration

Copy `.env.example` and set at least:
- `DATABASE_URL`;
- a stable `DREAM_SECRET`, or sessions end at every restart;
- `BASE_URL`, and `EARDE_PUBLIC_ORIGIN` as an `https://` origin;
- `EARDE_TRUSTED_PROXIES`, the proxy addresses whose `X-Forwarded-For` is
  trusted.

Everything else is optional and off by default: mail, Turnstile, the realtime
gateway, GitHub onboarding and analytics.

Keep secrets out of the repository and out of logs. Never set
`EARDE_LOG_TOKENS=1` on a shared host: it prints account-verification and
password-reset links.

## Reverse proxy

- Proxy everything to the application, and `/socket/` to the gateway, with
  WebSocket upgrade headers.
- Never expose the gateway's `/internal/publish` route. The application reaches it
  on loopback via `REALTIME_GATEWAY_URL`.
- Set `REALTIME_ALLOWED_ORIGINS` on the gateway to your public origin.
- The stylesheet is split into partials loaded through `@import`. Serve
  `/static/` with caching headers, or let the proxy serve it directly, so
  browsers do not fetch every partial on every page.

## Services

Run each process under a service manager that restarts it on failure, for
example systemd with `Restart=always`.

The gateway relies on this. If one of its essential subsystems crashes, the
whole gateway exits with a non-zero status rather than keep running degraded, and
clients reconnect and catch up over HTTP once it is back (see
`services/realtime_gateway/README.md`).

The gateway needs Erlang/OTP 27 or newer on the service's `PATH`.

## Upgrades

1. Build and test the new revision: `dune build`, `dune test`, and
   `gleam build && gleam test` for the gateway.
2. Apply new migrations with `dbmate up`. Migrations are forward-only.
3. Restart the gateway first, then the application.

Uploaded images are written to `static/uploads/`. Back it up together with the
database.
