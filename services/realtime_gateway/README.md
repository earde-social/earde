# realtime_gateway

Earde's WebSocket fanout gateway (Gleam, [Beryl](https://github.com/tylerbutler/beryl),
[Mist](https://github.com/rawhat/mist)). It runs next to the OCaml/Dream application
and is not a source of truth:

- Dream validates and commits every chat message to PostgreSQL, then publishes it
  here, best effort, for live delivery.
- Browsers can join a channel topic and send typing and shared-cursor updates. They
  cannot publish messages through the gateway.
- Presence, typing and cursors live only in memory. A restart clears them.

## Requirements

- Gleam 1.17 and Erlang/OTP 27 or newer (`gleam_json` uses OTP 27's `json` module).
  CI uses Gleam 1.17.0 and OTP 27.3.4.13.
- `rebar3`, used to build one Erlang dependency.

Dependency versions are locked in `manifest.toml`. Beryl is a Git dependency pinned
to a commit.

## Commands

```sh
gleam run                        # serve on 127.0.0.1:8090
gleam test                       # unit, protocol and failure-injection tests
gleam format --check src test
scripts/check-crash-contract.sh  # a subsystem crash must end `gleam run` non-zero
```

## Environment

| Variable | Required | Purpose |
|---|---|---|
| `REALTIME_TOKEN_SECRET` | yes | HMAC key for the signed per-topic tokens Dream mints for browsers. It must equal Dream's value. |
| `REALTIME_INTERNAL_SECRET` | yes | Shared secret Dream sends in `x-earde-internal` when publishing. It must equal Dream's value. |
| `REALTIME_ALLOWED_ORIGINS` | no | Comma-separated page origins allowed to open WebSockets, for example `http://localhost:8080`. When unset, only same-origin upgrades are accepted. |

The port (8090) and the loopback bind address are fixed in `src/realtime_gateway.gleam`.

## Routes

- `/socket/websocket`: browser WebSocket (Phoenix channel protocol, served by
  Beryl). It needs a valid, unexpired token for the joined topic.
- `/internal/publish`: Dream-only publish endpoint. It is gated by
  `REALTIME_INTERNAL_SECRET` and accepts only `new_msg` events.
- `/health`: returns `ok` while the process serves HTTP.

## Failure and recovery

The contract is documented on `realtime_gateway.start` and pinned by
`test/gateway_recovery_test.gleam` and `scripts/check-crash-contract.sh`.

- **Essential subsystems fail as a whole.** These are all linked to the main process:
  - Beryl's coordinator and handler registry;
  - presence, typing and cursors;
  - the HTTP server's supervisor.

  If one dies, the main process dies with it, and `gleam run` exits with a non-zero
  status. No healthy-looking gateway keeps running with a dead subsystem.
- **Restart is the service manager's job.** Run the gateway under one that restarts
  it on failure (for example systemd with `Restart=always`).
- **The listener recovers in place.** The listening socket and its acceptors belong
  to the HTTP server's own supervisor, which restarts them without affecting open
  connections.
- **Connections clean up after themselves.** When a socket closes, its presence,
  typing and cursor entries are removed immediately.
  - A connection that dies without closing is evicted by Beryl's heartbeat check,
    within the heartbeat timeout plus one check interval: 60 s + 30 s with the
    defaults.
  - Typing and cursor entries also expire on their own TTLs.
- **Clients recover without the gateway's help.**
  - Browsers reconnect with backoff, fetch a fresh token and rejoin.
  - They catch up on missed messages over HTTP from PostgreSQL
    (`/c/:slug/ch/:channel/messages.json?after_id=`).
  - Messages sent while the gateway is down, failing or hanging are still committed
    and served by that catch-up, as the `realtime_gateway_failure_db` suite in the
    OCaml tests shows.

## Deployment

The gateway binds to loopback only. Put it behind a reverse proxy that exposes
`/socket/` to browsers and never exposes `/internal/publish`.
