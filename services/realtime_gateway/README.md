# realtime_gateway

Earde's internal realtime WebSocket fanout gateway (Gleam / Beryl / Mist).

This is an internal service that runs alongside the Dream/OCaml app. Dream remains
the source of truth: it validates and persists chat messages to Postgres, then
best-effort publishes them here for live fanout. The gateway makes no auth decisions
and stores nothing — it only fans messages out over WebSocket.

## Run (local)

```sh
export $(grep -v '^#' ../../.env | xargs) && gleam run
```

## Build

```sh
gleam build
```

## Environment

- `REALTIME_INTERNAL_SECRET` — shared secret for the Dream → gateway internal publish.
- `REALTIME_TOKEN_SECRET` — HMAC key used to verify signed per-topic browser tokens.

## Routes

- `/socket/websocket` — browser WebSocket (Phoenix-compatible channels via Beryl).
- `/internal/publish` — Dream-only publish endpoint; gated by `REALTIME_INTERNAL_SECRET`.
- `/health` — liveness check.

## Deployment

Binds to loopback only. It is intended to sit behind a reverse proxy that exposes the
public WebSocket route while keeping the internal publish endpoint private.
