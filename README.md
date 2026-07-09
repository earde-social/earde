# Earde

Earde is a community platform for technical communities. It pairs live
text chat with durable, structured forum threads and a server-rendered, searchable
archive, so discussions that matter can be kept and found later instead of scrolling
away.

## Stack

- **OCaml / Dream / Postgres** — the main web application.
- **Server-rendered HTML** — pages render on the server and work without client-side
  routing.
- **Minimal vanilla JavaScript** — small, page-scoped scripts that enhance the SSR
  pages; no frontend framework or build chain.
- **Gleam / Beryl / Mist realtime gateway** — a small service for live message fanout.
- **Postgres** — the source of truth for durable data.

## Realtime model

- Dream owns authentication, permissions, moderation, and persistence.
- Postgres stores durable data (users, communities, channels, messages, threads).
- The Beryl gateway handles WebSocket fanout only.
- Browsers do not publish chat messages directly to the gateway. A message is posted
  to Dream, validated, and persisted; Dream then best-effort publishes it to the
  gateway for live delivery.
- If the gateway is unavailable, messages still persist through Dream and Postgres,
  and clients catch up over HTTP.

## Development

```sh
# Build and test the OCaml application
dune build
dune test

# Build the realtime gateway
cd services/realtime_gateway && gleam build
```

Running the app requires a `DATABASE_URL` pointing at a Postgres instance. Database
schema and migrations live in `db/`.
