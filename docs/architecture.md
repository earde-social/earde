# Architecture

Earde is one OCaml web application with a PostgreSQL database, plus a small Gleam
service for live WebSocket fanout. This document maps the code. Feature-level
design notes live in [docs/features/](features).

## Repository layout

| Path | Contents |
|---|---|
| `bin/main.ml` | Server entrypoint: configuration, middleware stack and routes |
| `bin/retry_posthog_deletions.ml`, `bin/check_posthog_config.ml` | Operator tools for the analytics integration |
| `lib/` | The application (one dune library, `earde`); every module has an `.mli` |
| `static/css/`, `static/js/`, `static/images/` | Stylesheets, page-scoped scripts, brand images |
| `db/migrations/` | Forward-only dbmate migrations: the schema's source of truth |
| `db/schema.sql` | Dump of a freshly migrated database, for review (checked in CI) |
| `services/realtime_gateway/` | The Gleam/Beryl/Mist WebSocket gateway |
| `test/` | Alcotest suites by domain, with shared fixtures in `test/support/` |
| `scripts/` | Schema check and the full (gated) test runner |

## Request flow

`bin/main.ml` wraps the router in this middleware stack, outermost first:

1. **Client address.** The client IP is taken from `X-Forwarded-For` only when the
   immediate peer is a trusted proxy (`Client_address`, `EARDE_TRUSTED_PROXIES`).
2. **Redaction.** Credential-bearing query values (`token`, `state`, `code`) are
   redacted before anything logs the target (`Request_target_redaction`).
3. **Logging, database and sessions.** Dream's logger, the Caqti connection pool,
   the secret, the session cookie's `Secure` flag (`Session_cookie_policy`), and
   SQL-backed sessions.
4. **Activity and badges.** Page-view recording and the notification badge
   (`Activity_middleware`, `Notification_badge`).
5. **Router.** The original target is restored for the router.

CSRF protection uses Dream's form tokens (`Csrf_field`). The consent endpoint uses
JSON with origin checks instead. Rate limits (`Rate_limit_middleware`,
`Rate_limit_store`) wrap the authentication POSTs and fail closed.

## Application modules

Modules are named by feature and layer. A feature usually has most of these:

- **`*_handlers`**: HTTP handlers. They authenticate, authorize, parse the form
  (`*_form` modules), call a store or read model, and choose the response.
  Shared helpers are in `Handler_support`, `Admin_authority` (durable
  `users.is_admin`) and `Community_read_gate` (private-community reads).
- **`*_store`**: transactional writes and their authorization predicates, in SQL.
  Lock ordering and concurrency rules are documented in each `.mli`.
- **`*_read_model`**: authorized reads shaped for one page.
- **`*_pages`**: rendering. Pages are built through `Html`, whose `Html.t` is
  escaped by construction (see
  [features/safe-rendering.md](features/safe-rendering.md)). Shared layout lives
  in:
  - `Page_shell` (application chrome);
  - `Community_shell` (community surfaces);
  - `Community_settings_shell`, `Components` and `Post_cards`.
- **Pure domain modules** such as `Project_identity`, `Project_home_relation`,
  `Shared_thread_placements` and `Realtime_generation`. They hold the rules that
  need no IO and are tested without a database.

Main feature areas:
- accounts and authentication (`auth_*`, `credential_store`, `pending_signup_store`,
  `email`, `turnstile`);
- communities and their structure (`community_*`, `section_store`, `channel_store`);
- posts, comments and votes;
- chat (`chat_*`, `realtime*`);
- moderation (`moderation_*`, `mod_log_store`, `report_store`, `community_ban_store`);
- notifications;
- GitHub project onboarding and project homes (`github_*`, `project_*`, `network_community_*`);
- community connections;
- Shared Threads (`shared_thread_*`);
- analytics (`analytics*`, `posthog_*`).

## Data

- **PostgreSQL** holds all durable state. Sessions are stored in the
  `dream_session` table.
- **Schema changes** are new, timestamped migrations in `db/migrations`. Applied
  migrations are never edited. After adding one, regenerate `db/schema.sql` with
  `dbmate up` or `dbmate dump`.
- **Authorization** is decided from durable state:
  - global administrators are `users.is_admin`, never a session claim;
  - community roles come from `community_moderators`;
  - bans are enforced by store predicates, not by the UI.

## Realtime

- **Publish after commit.** A chat message is written to PostgreSQL first. After
  the commit, Dream publishes it to the gateway's internal endpoint, best effort
  (`Realtime.publish_chat_message`, bounded by a short timeout). A failed publish
  loses nothing.
- **Signed topic tokens.** Browsers connect to the gateway with a signed,
  short-lived, per-topic token (`Realtime_token`). The topic includes a
  per-community access generation, which database triggers bump whenever read
  access can shrink, so revoked users stop receiving new messages. See
  [features/realtime-access.md](features/realtime-access.md).
- **Catch-up.** Clients catch up with `GET /c/:slug/ch/:channel/messages.json?after_id=`
  whenever they (re)connect.
- **The gateway** (`services/realtime_gateway`) stores nothing and makes no
  authorization decisions beyond verifying tokens. Its failure and recovery
  contract is described in its [README](../services/realtime_gateway/README.md).

## Browser code and styles

- **Scripts.** JavaScript is loaded per page:
  - `chat_live.js`: live chat;
  - `thread.js`: thread pages;
  - `analytics.js`: the consent-gated analytics;
  - `phoenix.js`: the vendored Phoenix channel client.

  User data reaches scripts only through `data-*` attributes, never inline
  script source.
- **Stylesheets.** Pages link one stylesheet, `static/css/earde.css`. It imports,
  in cascade order:
  - `base/`: the design system (tokens, reset, shell, components, responsive
    rules);
  - `routes/chrome.css`: shared route chrome;
  - `routes/<area>.css`: per-route integration skins, each scoped under the
    page's body class `launch-<route>`.

  A route joins shared chrome by being listed in that rule's `:is()` list. The
  mobile gate panel for desktop-only routes is `static/css/mobile-gate.css`.

## Tests

- **Layout.** Suites live in `test/<domain>/` and are registered by
  `test/test_earde.ml`. Shared fixtures are in `test/support/`.
- **DB-free and gated cases.** Cases that need a database call
  `Db_fixture`-style helpers and are skipped unless `EARDE_TEST_DATABASE_URL` is
  set. `scripts/test-gated.sh` runs everything and fails if any case was
  skipped.
- **Census tests** read the production sources and stylesheet to enforce
  structural rules. Examples:
  - `Html.trusted` is used only for the CSRF field;
  - no legacy stylesheet is served;
  - every community-shell page class gets the shared chrome.
