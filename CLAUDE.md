# Earde — Engineering Guide

This is a concise engineering guide for working on Earde. It describes the project
architecture and the conventions to follow when changing it.

## Product

Earde is a community platform for technical communities. It combines live text chat
with durable, structured forum threads and a server-rendered, searchable archive, so
valuable discussion can be kept and found later rather than scrolling away.

## Architecture overview

- **OCaml / Dream / Postgres**, server-rendered HTML. Pages render on the server and
  are useful before any JavaScript runs.
- **Minimal, page-scoped vanilla JavaScript** that enhances the SSR pages. No SPA, no
  client-side routing, no frontend build chain.
- **Postgres is the source of truth** for durable data: users, sessions, communities,
  channels, chat messages, threads, and moderation state.
- **Realtime gateway (Gleam / Beryl / Mist)** handles live WebSocket fanout only. It
  is not a source of truth: Dream validates and persists a message to Postgres first,
  then best-effort publishes it to the gateway for live delivery. If the gateway is
  down, messages still persist and clients catch up over HTTP.

## Architecture style

- Prefer a few cohesive, feature-oriented macro-modules over many tiny files. The main
  modules today are `lib/db.ml`, `lib/handlers.ml`, `lib/pages.ml`, and
  `lib/components.ml`.
- A new top-level module is fine only when it is cohesive and feature-oriented; avoid
  scattering a single feature across many tiny files without a strong reason.
- Group related logic with inner modules rather than splitting into more files.

## Interface discipline

- Keep every `.mli` aligned with its `.ml`.
- Expose only the public surface other modules need; keep implementation details
  private.

## OCaml and database style

- IO, database, and session operations use `Lwt`.
- Use explicit `result` types for fallible operations.
- Match the existing Caqti patterns. For large Caqti decoders/encoders, use nested
  tuples rather than changing the style.
- Prefer closed variants for values that drive dynamic SQL or sorting.
- Escape all rendered output with the existing HTML and URL escaping helpers.
- Comments should explain *why*, not restate *what*.

## SSR and JavaScript rules

- Pages should be useful before any JavaScript runs, and public archive/content pages
  should remain crawlable.
- Keep JavaScript small, page-scoped, and loaded only on the pages that need it.
- No SPA, client-side routing, hydration architecture, or frontend build chain.
- Do not add npm, Vite, webpack, esbuild, PostCSS, or Sass unless explicitly approved.

## Realtime rules

- Postgres and Dream remain the source of truth for durable data, authentication,
  validation, permissions, and moderation.
- The Gleam / Beryl / Mist gateway is for live WebSocket fanout only.
- Browsers must not publish chat messages directly to the gateway; messages go through
  Dream, which validates and persists them first.
- A dropped live event must be recoverable from Postgres via HTTP catch-up.
- Do not move durable data — users, posts, comments, chat messages, votes, karma,
  moderation logs, threads, or notifications — into ephemeral realtime infrastructure.

## Migration rules

- Migrations are forward-only. Add a new timestamped migration; do not edit an
  already-applied migration.
- Prefer additive, backward-compatible schema changes and soft-deletes.
- Do not run destructive database commands (DROP, TRUNCATE, destructive migrations,
  data wipes) without explicit human approval.

## Testing

- New logic-heavy features should ship with tests, runnable with the normal local
  test setup.
- Run `dune build` and `dune test` before considering a change complete.
- Run `gleam build` (in `services/realtime_gateway`) when touching the realtime
  gateway.

## Scope guardrails

- Do not introduce a native desktop app, Electron, voice/video, or LiveKit into the
  core product unless explicitly requested.
- Do not split the app into new services or add network hops unless explicitly
  requested.
- Prefer reusing existing modules and patterns over adding new dependencies or
  rewrites; introduce heavy frontend frameworks or new dependencies only with explicit
  human approval.
- No production deploys unless explicitly requested.

## Security and privacy

- Avoid logging secrets, tokens, or unnecessary personal data. Never print or commit
  secrets; keep `.env` untracked.
- Analytics and tracking must be disclosed and implemented deliberately. Do not add
  new tracking without an explicit product decision.
