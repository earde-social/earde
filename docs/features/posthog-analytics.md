# PostHog Analytics — Implementation Specification

Status: **specification only** — no code implemented. Branch: `feature/posthog-analytics`.
Based on the codebase audit of 2026-07-21. Line numbers are anchors as of that audit.

Target PostHog environment (verified, read-only):

| Item | Value |
|---|---|
| Project | Earde Production, id `229260`, **EU Cloud** |
| Ingest host | `https://eu.i.posthog.com` |
| SDK assets host | `https://eu-assets.i.posthog.com` |
| UI host | `https://eu.posthog.com` |
| Current state | Zero events ingested; session replay, autocapture, web vitals already enabled project-side |

## 1. Architecture context

Earde is two deployables sharing one domain:

- **Wasp landing project** (separate repository, audited 2026-07-22): **Wasp
  0.24.0** using the TypeScript project specification (`main.wasp.ts`), a single
  route `/` with `prerender: true`. The landing is prerendered to static HTML at
  build time but **hydrates a full React client runtime** in production. In
  production, Nginx serves it at `/`. It is instrumented **later, in its own
  repository** (§11), using the **same PostHog project and the same identity
  model** defined here — and, because it has its own Vite build chain, it uses
  the **`posthog-js` npm package with consent-gated initialization**; the Dream
  repo's no-npm constraint does not apply to it.
- **This Dream/OCaml application**, which starts at `/feed` and owns all application
  routes. The in-repo `Dream.get "/"` 302 → `/feed` (`bin/main.ml:105`) is a
  dev-convenience/fallback; production traffic to `/` never reaches Dream.

Consequences for this spec:

- The following are the **cross-repo contract** and must match exactly in both
  browser integrations:
  - the same PostHog project token and EU hosts (§8);
  - compatible `localStorage+cookie` persistence (§2.2), so the anonymous
    distinct ID survives navigating from `/` to `/feed` on the one shared
    origin;
  - the same consent cookie `earde_analytics_consent` (`Path=/`, JS-readable —
    §9), and the same sanitization rules (§2.3);
  - the same authenticated distinct-ID scheme `user:<database_user_id>` (§4.1);
  - the same identify/reset/group-clear rules (§4.2) — required because logout
    and account deletion redirect to `/`, which the landing serves in
    production.

  A visitor who lands on `/`, then signs up in the app, becomes one PostHog
  person.
- Nothing in this repo instruments the landing; the landing-side work is scoped
  in §11.

Division of responsibilities:

- **Browser SDK** (in this repo: snippet in the shared SSR layout — no npm, no
  build chain, per this repo's `CLAUDE.md`; that constraint is Dream-specific —
  the landing uses `posthog-js`, §1/§11): sanitized page views/leaves, Web
  Analytics, autocapture, heatmaps,
  Web Vitals, session replay, JS error tracking, and UI-only events that do not
  correspond to a persisted domain mutation.
- **Server (Cohttp/Lwt HTTP Capture API)**: the nine domain events, captured
  best-effort and non-blocking, only after the Postgres mutation succeeds.
  Analytics failure must never fail the product operation. No OCaml PostHog SDK
  dependency.

## 2. Browser-side integration

### 2.1 Snippet placement

- Single injection point: **`Components.layout`** head region
  (`lib/components.ml:320–335`), after the Tailwind CDN script at `:333`. This
  covers every page and all three chrome variants (`` `Site``/`` `App``/`` `Auth``).
- The only layout bypass, `Pages.hq_dashboard_page` (`lib/pages.ml:4957`), is being
  **removed** (§7) and must NOT get a snippet.
- Emission is conditional on `POSTHOG_ENABLED` (§8). Config (token, api host, ui
  host) reaches the browser via HTML-escaped `data-*` attributes rendered by
  `layout`, consumed by a new `static/js/analytics.js` (global, loaded from
  `layout` like the existing inline global script; IIFE, matching `chat_live.js`
  style).

### 2.2 SDK initialization (decisions, not code)

| Concern | Decision |
|---|---|
| Page views | `capture_pageview: false` — **manual capture only** (§2.3) |
| Page leaves | `capture_pageleave: true` (URLs pass through the sanitizer) |
| Autocapture | on |
| Heatmaps | on |
| Web Vitals | on (project flag already enabled) |
| Error tracking | exception autocapture on |
| Session replay | on, with masking per §6 |
| Replay network capture | `recordHeaders: false`, `recordBody: false` — chat catch-up JSON contains message bodies and must never enter replay |
| Group context | declared per page load via a data attribute; set or cleared **before** the manual pageview capture (§5.3) |
| Persistence | not initialized at all before consent (§9); `localStorage+cookie` after opt-in |
| Global property scrub | a `sanitize_properties` hook strips query string and fragment from `$current_url`, `$referrer`, `$pathname` on **every** event, as defense in depth |

### 2.3 URL sanitization (hard requirement)

Tokens travel in query strings in this app (`/confirm-email?token=…`,
`/verify?token=…`, password-reset links); the repo already has
`redact_token_middleware` (`bin/main.ml:91`) for exactly this reason, and search
terms travel in `/search?q=…`.

- Manual `$pageview` capture sends `origin + pathname` only — never
  `location.search`, never `location.hash`.
- The `sanitize_properties` hook (§2.2) enforces the same rule for autocapture,
  pageleave, replay metadata, and error events.
- Rule stated once, applied everywhere: **no verification, confirmation,
  reset-password, or other secret token may ever reach PostHog** — not in URLs,
  not in properties, not in replay.

### 2.4 UI-only events

Mechanism: `analytics.js` may capture events for interactions that do not persist a
domain mutation. Rule: a UI-only event must never duplicate a server-side domain
event (§3).

Initial set (deliberately minimal):

- `search_performed` — captured on the search results page from server-rendered
  `data-*` attributes (result count, active tab, page number). The query text is
  **never** included; the sanitized URL already excludes `?q=`.

## 3. Server-side integration

### 3.1 Capture module

New cohesive module **`lib/analytics.ml` + `lib/analytics.mli`** (precedent:
`lib/turnstile.ml` — env-driven optional external HTTP service).

- Public surface (`analytics.mli`) — **consent is enforced by construction**;
  the module exports no capture path that bypasses the consent check:

  ```ocaml
  val capture_if_consented :
    Dream.request ->
    distinct_id:string ->
    event ->                     (* closed variant, §5 *)
    unit

  val sync_person_after_consent_grant :
    distinct_id:string ->
    person_properties ->         (* closed record, §4.3 *)
    unit

  val attempt_person_deletion :  (* §3.3 durable data lifecycle; not consent-gated *)
    job_id:int ->
    distinct_id:string ->
    unit
  ```

  `capture_if_consented` reads the `earde_analytics_consent` cookie from the
  request and returns without side effects unless it is `granted` (and
  `POSTHOG_ENABLED` is true). Because handlers must hand over the request to
  capture anything, they cannot emit analytics without passing through the
  consent check. The raw HTTP-capture function stays private to the module.

  Two narrowly scoped exceptions to request-based gating exist, neither of
  which is a generic event-capture bypass:

  - `sync_person_after_consent_grant` deliberately does **not** inspect the
    request cookie: at the moment consent is granted, the `granted` value
    exists only in the outgoing `Set-Cookie` response header — `Dream.request`
    still carries the old state, so a request-based check would wrongly refuse.
    Its scope is minimal: it can only emit a `$set` of the closed §4.3
    `person_properties` record for one distinct ID — no arbitrary events — and
    it may be called only by `analytics_consent_handler`, after `state` has been
    validated as exactly `granted`, the §9 request checks (JSON-only body,
    `Origin`/`Sec-Fetch-Site`) have passed, and the authenticated user's
    properties have been loaded (§9). Denied consent never
    calls it. All ordinary domain events still go through
    `capture_if_consented request`.
  - `attempt_person_deletion` operates on an already-committed deletion job
    (§3.3), identified by its job id and immutable distinct ID — it never reads
    the (by then anonymized) user record, and it removes data rather than
    collecting it.
- All exported functions return `unit` immediately (best-effort, non-blocking).
  Internally: build the JSON payload, then `Lwt.async` a Cohttp POST to
  `POSTHOG_API_HOST` `/capture/` with the project token. Short timeout (2–5 s), no
  retries in v1, failures logged at warning level and swallowed. The `Lwt.async`
  body catches all exceptions so `Lwt.async_exception_hook` never fires.
- Call sites follow the existing fire-and-forget precedent
  (`lib/handlers.ml:1294`) and the existing rule of releasing the DB connection
  before third-party HTTP (comments at `lib/handlers.ml:301–304, 403`).

### 3.2 Domain events — capture points

Capture happens at the `Ok …` match arm, **after the DB mutation succeeds** and
before the response, always via `capture_if_consented request …` (§3.1). For
logout-adjacent flows, before `Dream.invalidate_session`.

| Event | Handler | Success anchor | Notes |
|---|---|---|---|
| `signup_confirmed` | `confirm_email_handler` `lib/handlers.ml:338` | `` `Confirmed`` arm `:346` | Requires `Db.pending_signup_confirm` to also return the new user id (§5.2) — today only the username is in scope. No session exists here; distinct ID comes from the returned id. **The confirmation link may be opened in a different browser or device than the one that consented; if the consent cookie is absent there, `signup_confirmed` is not captured.** Accepted gap: the person is then first created at `login_succeeded`. |
| `login_succeeded` | `login_handler` `:360` | after checks pass, at session-set `:377–379` | `id`, `is_admin` in scope |
| `community_joined` | `join_community_handler` `:1541` | `:1564–1565` | full `community` record in scope (id, slug, visibility) |
| `community_left` | `leave_community_handler` `:1570` | `:1582–1583` | only numeric `community_id` in scope; v1 sends id without slug |
| `chat_message_sent` | `send_message_handler` `:1222` | `Ok message` `:1284–1285` | single capture point covers **both** response branches (JSON at `:1303` / redirect at `:1310`); `response_mode` distinguishes them |
| `post_created` | `create_post_handler` `:2211` | `:2305–2306` | mention fan-out at `:2308–2317` supplies `has_mention` |
| `comment_created` | `create_comment_handler` `:3017` | `:3068–3069` | requires `INSERT … RETURNING id` change (§5.2) to include `comment_id` |
| `thread_promoted` | `start_thread_create_handler` `:1446` | `Ok post_id` `:1527–1528` | context message ids at `:1504` supply promoted counts |
| `account_deleted` | `delete_account_handler` `:3755` | after the §3.3 transaction commits, **before** `invalidate_session` `:3765` | the handler's bare `Db.anonymize_user` call (`:3763`) is replaced by the atomic `Db.anonymize_user_and_enqueue_posthog_deletion` (§3.3); capture and the immediate deletion attempt both run only after that commit |

Search is deliberately **not** a server event: it is a GET page render, covered by
the sanitized `$pageview` plus the browser-side `search_performed` (§2.4).

### 3.3 Account deletion — PostHog data lifecycle

Verified against current PostHog docs (read-only):
[data deletion](https://posthog.com/docs/privacy/data-storage#data-deletion),
[persons API](https://posthog.com/docs/api/persons).

A single failed network call must not be able to leave personal data in PostHog
indefinitely, so deletion is **durable**: a small job table plus a bounded retry
operation, not a one-shot best-effort call.

**Deletion job table** — new forward-only, additive migration
(`db/migrations/<timestamp>_add_posthog_person_deletion_jobs.sql`):

```
posthog_person_deletion_jobs
  id               BIGSERIAL PRIMARY KEY
  distinct_id      TEXT NOT NULL          -- immutable "user:<id>"; deliberately
                                          -- NO foreign key to users, so the job
                                          -- can never block or complicate local
                                          -- account deletion
  status           TEXT NOT NULL          -- 'pending' | 'completed'
                                          -- (closed variant in OCaml)
  attempts         INT NOT NULL DEFAULT 0
  last_error       TEXT                   -- HTTP status / short error class only
  created_at       TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
  last_attempt_at  TIMESTAMP
  completed_at     TIMESTAMP
```

**Atomic anonymization + job enqueue.** `delete_account_handler` replaces its
bare `Db.anonymize_user` call (`lib/handlers.ml:3763`) with **one transactional
DB operation**, conceptually:

```
Db.anonymize_user_and_enqueue_posthog_deletion :
  user_id:int -> (job_id * distinct_id) result
```

Inside a single PostgreSQL transaction it:

- locks/loads the required user identity (`SELECT … FOR UPDATE`);
- derives the immutable distinct ID `user:<database_id>`;
- anonymizes the local user (the same rewrite as today's `Db.anonymize_user`,
  `lib/db.ml:545–552`);
- inserts the `pending` `posthog_person_deletion_jobs` row;
- **commits both changes together or rolls back both.**

A process crash therefore can never separate the anonymization from its
deletion request — there is no window in which the account is anonymized but no
durable deletion job exists. The external PostHog HTTP request **never runs
inside this transaction**.

**After the transaction commits successfully** (`Ok (job_id, distinct_id)`):

1. Optionally `capture_if_consented request ~distinct_id Account_deleted` —
   the original incoming request still carries its consent cookie, so the
   normal request-based gate applies (skipped without consent).
2. `Analytics.attempt_person_deletion ~job_id ~distinct_id` triggers an
   **immediate asynchronous best-effort attempt** for the newly created job —
   it receives the job id and immutable distinct ID and never touches the
   now-anonymized user record.
3. The handler returns the normal successful account-deletion response
   (`invalidate_session`, redirect to `/`).

**The deletion attempt** (whether immediate or via the retry operation below)
runs against the **private REST API on `POSTHOG_UI_HOST`**
(`https://eu.posthog.com`), never the ingest host:

- resolve the person UUID:
  `GET /api/projects/$POSTHOG_PROJECT_ID/persons?distinct_id=<distinct_id>`;
- delete person **and** events:
  `DELETE /api/projects/$POSTHOG_PROJECT_ID/persons/<uuid>?delete_events=true`;
- auth: `Authorization: Bearer $POSTHOG_PERSONAL_API_KEY` — a **personal API
  key** (`phx_…`), minimally scoped to person read/write on this project (§8).
  Server-only secret: never in browser code, never rendered into HTML, never
  logged. The project id comes from `POSTHOG_PROJECT_ID` (§8) — never hardcoded
  in `lib/analytics.ml`.
- The job is marked `completed` **only** after PostHog accepts the deletion
  request (`202`), or when the persons lookup shows the person is already
  absent (treated as successfully completed — covers never-consented users and
  re-runs).
- On network or HTTP failure the job stays `pending` (attempts incremented,
  `last_error` and `last_attempt_at` updated).

**Bounded maintenance operation** — `retry_pending_posthog_deletions`, suitable
for a manual deployment command now and a timer later:

- processes a fixed batch (e.g. 25 oldest pending jobs) per invocation;
- increments `attempts` and updates `last_attempt_at` / `last_error` per job;
- runs outside the request path — it never blocks normal user requests;
- logs only numeric/local identifiers (job id, numeric user id) and safe HTTP
  status information — never email or username;
- treats an already-absent PostHog person as successfully completed.

Ordering and failure behavior:

- **Local account deletion never fails because PostHog is unavailable** — the
  capture and the immediate attempt are async after the transaction commits,
  all exceptions caught, and the durable pending job provides eventual
  remediation.
- The browser resets its PostHog identity through the §4.2 mechanism: the
  post-deletion redirect renders an anonymous page, so `analytics.js` calls
  `posthog.reset()`.
- `delete_events=true` also removes the person's prior events — including the
  just-captured `account_deleted` event once the purge runs; accepted (a
  person-less aggregate counter can be added later if a durable metric is
  wanted).
- Data-lifecycle statement (mechanics, not a legal claim): Earde does **not**
  promise immediate physical purging — PostHog acknowledges the deletion
  request and purges ClickHouse events asynchronously (batched, roughly
  weekly). What Earde guarantees is that it **retains a durable pending
  deletion request until PostHog accepts it**: the person profile is removed at
  acceptance, the events at PostHog's next purge cycle, and an unaccepted
  request keeps being retried rather than silently dropped.

## 4. Identity

### 4.1 Distinct ID

- Authenticated distinct ID: **`user:<database_user_id>`** (e.g. `user:42`).
  Server events and browser identify use this exact string, so they resolve to one
  person. The Wasp landing will adopt the same scheme.
- The signed realtime chat token (`lib/realtime_token.ml:39–48`,
  `data-signed-token`, chat pages only) is **not** an analytics identity source and
  must not be read by `analytics.js`.

### 4.2 identify / reset in an SSR, redirect-heavy app

Session state lives server-side (`Dream.sql_sessions`, fields set at
`lib/handlers.ml:377–379`); user identity is not in the DOM today. Mechanics:

- `Components.layout` newly emits, for authenticated pages (it already receives
  `?request` and reads the session at `lib/components.ml:224–226`), a body-level
  attribute carrying `user:<id>` (and optionally the public username).
- On every page load, `analytics.js` (post-consent):
  - attribute present and `posthog.get_distinct_id()` differs → call
    `posthog.identify("user:<id>")`. Because every login ends in a 302 to a
    layout-rendered page, this runs on the **first page after login** and merges
    the anonymous browsing history into the person. The browser `identify()`
    call carries **no `$set`** — person properties are set server-side (§4.3) so
    email never transits the DOM or client-side JavaScript.
  - attribute absent but the persisted distinct ID starts with `user:` → call
    `posthog.reset()`. This covers **logout** and **account deletion** (both 302
    to `/`, both rendered anonymous), with no event-time JS needed — but note
    that in production `/` is served by the **Wasp landing**, so the page that
    actually executes this reset is the landing's integration, not
    `analytics.js` (see the contract below).
- `signup_confirmed` (server, `user:<id>`) creates the person before the first
  login when consent allows (§3.2); the first post-login identify merges
  pre-signup anonymous activity into it.

These rules are part of the **cross-repository contract** (§1): both browser
integrations — Dream's `analytics.js` and the landing's client module (§11) —
must implement, identically:

- authenticated identity attribute present → `posthog.identify("user:<id>")`;
- no authenticated identity, but the persisted PostHog distinct ID begins with
  `user:` → `posthog.reset()`;
- group context set from the page's group attribute, and cleared/reset on
  anonymous/global pages (§5.3).

The landing never renders an authenticated identity attribute, so its side of
the contract reduces to the anonymous-side reset check (performed after PostHog
initialization) and always-cleared group context. This is required — not
optional hardening — because production logout and account deletion redirect to
`/`, which Nginx serves from the landing rather than Dream; without it, a
logged-out user's subsequent anonymous browsing would keep merging into their
old person.

### 4.3 Person properties

After analytics consent and authenticated identification, these PostHog
**person properties** are set when available:

| Person property | Source |
|---|---|
| `username` | session / login lookup |
| `email` | `users.email` — extend the login lookup to select it if not already |
| `signup_date` | `users.created_at`, ISO 8601 |
| `is_admin` | login lookup (`lib/handlers.ml:371`) |

Mechanics and rules:

- They are attached **server-side** as `$set` on `signup_confirmed` (when
  captured) and `login_succeeded` — inside `capture_if_consented`, so they are
  consent-gated like everything else and are refreshed at each login.
- They are additionally synchronized **immediately when consent is granted**:
  the `POST /analytics/consent` handler (§9) loads the current user's values and
  calls `sync_person_after_consent_grant` (§3.1 — the narrowly scoped consent
  transition function, since the just-granted cookie exists only in the outgoing
  response, not on the current request), so an already-logged-in user who
  accepts analytics gets their person properties without logging out and back
  in.
- They are **person properties, not event properties**: they never appear in the
  §5.1 custom-event allowlist, and handlers cannot attach them (closed variant).
- They must never appear in session replay, autocapture element text, URLs, or
  arbitrary event properties. Email is not rendered in the DOM outside the
  masked admin table (§6), the browser never receives it for analytics purposes,
  and the URL sanitizer (§2.3) already strips query strings where such values
  could otherwise travel.
- On account deletion they are removed with the person (§3.3).

## 5. Event properties

### 5.1 Closed allowlist

Events are modeled as a closed OCaml variant in `lib/analytics.ml`, so the
compiler enforces the allowlist — handlers cannot attach arbitrary properties.
Permitted properties (per event, where applicable):

| Property | Type | Used by |
|---|---|---|
| `user_id` | int | all authenticated events |
| `community_id` | int | joined/left/chat/post/comment/promoted |
| `community_slug` | string | joined/chat/promoted (in scope); omitted where only the id is in scope (left, post, comment) in v1 |
| `community_visibility` | string | community_joined |
| `channel_id` / `channel_slug` | int / string | chat_message_sent, thread_promoted |
| `section_id` | int | post_created, thread_promoted |
| `post_id` | int | post_created, comment_created, thread_promoted |
| `comment_id` | int | comment_created (needs §5.2) |
| `parent_comment_id` | int option | comment_created (top-level vs reply) |
| `message_id` | int64 | chat_message_sent; seed message of thread_promoted |
| `promoted_message_count` | int | thread_promoted (seed + context ids) |
| `promoted_participant_count` | int | thread_promoted (distinct authors among promoted messages; if not derivable at `:1504`, extend `Db.start_thread_from_chat`'s return — otherwise omit in v1) |
| `surface` | string | where the action originated (`feed`, `thread`, `chat`, `search`, `email_link`, …) |
| `user_role` | string | `member` / `mod` / `top_mod` / `admin` where already in scope; no extra lookups |
| `content_length` | int | chat/post/comment (length only) |
| `has_link` | bool | post_created (url field present) |
| `has_mention` | bool | post_created, comment_created |
| `result_count` | int | search_performed (browser-side) |
| `response_mode` | string | chat_message_sent: `json` \| `redirect` |

Hard rule: **no chat, post, or comment bodies, no titles, no search query text, no
emails, no tokens** in any custom event property. `username`, `email`,
`signup_date`, `is_admin` are person properties (§4.3), never event properties.

### 5.2 Required DB-layer changes (additive)

- `Db.create_comment` (`lib/db.ml`, called at `lib/handlers.ml:3068`): change the
  insert to `INSERT … RETURNING id` so it yields `Ok comment_id` instead of
  `Ok ()`. Update `db.mli` and the single call site.
- `Db.pending_signup_confirm` (`lib/handlers.ml:345`): extend the success payload
  from `` `Confirmed of username`` to also carry the new user id (`RETURNING id`
  on the user insert), so `signup_confirmed` can use `user:<id>`.

Both are backward-compatible query changes, no migration needed.

### 5.3 Community group analytics

Mechanics verified against current PostHog docs (read-only):
[group analytics](https://posthog.com/docs/product-analytics/group-analytics),
[capture API `$groupidentify`](https://posthog.com/docs/api/capture),
[frontend vs backend groups](https://posthog.com/tutorials/frontend-vs-backend-group-analytics).

- **Group type:** `community` (the first of the 5 available group types).
- **Stable group key:** `community:<database_community_id>` (e.g. `community:7`).
  Slugs are mutable; the numeric id is not.

**Server-side.** Backend capture is stateless, so group context is attached
per event: every §3.2 event associated with a community (`community_joined`,
`community_left`, `chat_message_sent`, `post_created`, `comment_created`,
`thread_promoted`) carries, in its capture payload:

```json
"$groups": { "community": "community:<id>" }
```

Group **properties** are not sent on every event; they are set via a dedicated
`$groupidentify` event to the same `/i/v0/e/` endpoint (`$group_type:
"community"`, `$group_key`, `$group_set: {…}`), emitted where the full community
record is in scope: community creation, community settings update, and
`community_joined`. Allowlisted group properties (closed, like §5.1):

| Group property | Notes |
|---|---|
| `community_id` | int |
| `community_slug` | current slug |
| `community_name` | display name (not private content) |
| `community_visibility` | `public` / `private` |
| `created_at` | when available on the community record, ISO 8601 |

**Browser-side.** `posthog.group()` is session-sticky: once called, it
associates **all subsequent events** (pageviews, autocapture, UI events) with
that group and persists across page loads. In an SSR app the page itself must
therefore declare its group context on every load:

- Community-scoped shells (community home, channel/chat, thread, manage pages —
  they all receive the community record) render a
  `data-analytics-group="community:<id>"` attribute alongside the identity
  attribute of §4.2.
- On every page load, before capturing the manual `$pageview`, `analytics.js`:
  - attribute present → `posthog.group('community', key)` (key only — group
    properties are owned by the server via `$groupidentify`);
  - attribute **absent** (global pages: `/feed`, search, settings, profiles) →
    clear the group context (`posthog.resetGroups()`), so the previously visited
    community is never carried onto global pages.
- `posthog.reset()` at logout/deletion (§4.2) clears group state as well.

Private communities: group analytics still applies (the group key and
allowlisted properties are not content), while §6 blocks their page content
from replay.

## 6. Session replay masking

Strategy: **selector-based text masking + built-in block class**, so it covers both
server-rendered nodes and nodes created at runtime by `chat_live.js`
(`static/js/chat_live.js:232–276` uses the same `.cs-msg-*` classes as SSR
`render_message`, `lib/pages.ml:1032–1088`). `maskAllInputs: true` covers every
input and textarea. General layout, navigation, click targets, scroll, dialogs and
feature flows stay observable — no mask-all-text.

| Content | Mechanism |
|---|---|
| Chat message bodies + authors, typing, presence | `maskTextSelector`: `.cs-msg-text`, `.cs-msg-author`, `#chat-typing`, `.cs-presence-name` (works for SSR and live-inserted nodes) |
| Post titles/bodies | `.ft-title`, `.ft-preview` (`lib/components.ml:1104/1072`), `.th-title`, `.th-body` (`lib/pages.ml:1723/1477`), `.sr-row-title` |
| Comments | `.ctext` (`lib/pages.ml:1689`), `[id^="comment-content-"]` (thread + legacy `:3597`), `.sr-row-excerpt` |
| Notifications | `.account-notif-msg` (`lib/pages.ml:4354`) |
| Moderation reasons / report excerpts | `.cm-table-reason` (`lib/pages.ml:3227`) plus a new `ph-mask` class (below) on report/mod-log excerpts and ban-reason blocks (`lib/pages.ml:3120–3123, 3236, 3262`; `lib/components.ml:619, 647, 691`) |
| Emails in the DOM | `.admin-cell-muted` (`lib/pages.ml:4828, 4860`) — additionally the whole `.admin-table` may use the built-in `ph-no-capture` block class, since the admin dashboard has no replay value |
| Form inputs & textareas | `maskAllInputs: true` (covers `#su-email`, `#li-identifier`, `#fp-email`, bio textarea, ban reasons) |
| Search input | covered by `maskAllInputs`; echoed query in results header gets `ph-mask` |
| Profile bio | `.account-bio` (`lib/pages.ml:4225`) |
| Private-community content | the main content container of private-community pages gets PostHog's built-in **`ph-no-capture`** class (rendered by the shell when `community.visibility = private`), blocking those regions from replay entirely on top of the class masking |

Gap-filling convention: where a private-content element has no stable class today
(feed card title/preview `lib/components.ml:799/804`, legacy post body
`lib/pages.ml:3655`, report excerpts), add the dedicated **`ph-mask`** class in the
markup and include `.ph-mask` in `maskTextSelector`. Usernames elsewhere
(`render_author`, `lib/components.ml:510`) remain visible: they are public handles.

Person properties (§4.3 — including email) live only in the PostHog person
profile via server-side `$set`; they are never rendered into the DOM for
analytics purposes and never appear in autocapture element text or URLs, so
they create no new masking surface. The only emails in the DOM remain the admin
table cells masked above.

## 7. Existing analytics removal

Current state: `analytics_middleware` (`lib/handlers.ml:4064–4146`, wired
`bin/main.ml:99`) both logs page views **and** performs the presence touch. The
touch is load-bearing: `Db.touch_user_active` (`lib/db.ml:2341–2349`) is the only
writer of `users.last_active_at`, which `demote_inactive_mods` (`lib/db.ml:2171`)
depends on.

Plan, in order:

1. **Extract presence first (behavior-neutral).** New, clearly named
   `presence_middleware` in `lib/handlers.ml` containing only the
   session-`user_id` → `Db.touch_user_active` logic, wired in `bin/main.ml` inside
   `Dream.sql_sessions`. `touch_user_active` moves out of the `Db.Analytics`
   module namespace into a presence-named home (`db.ml` `:2341–2349`, aliases
   `:3161`, `db.mli:417/594`). `analytics_middleware` shrinks to page-view logging
   only. `users.last_active_at` and its consumers are untouched.
2. **Stop writing `page_views`** once PostHog browser capture is verified: delete
   the reduced `analytics_middleware` — removing `Db.log_page_view` insertion, the
   `MD5(ip ^ ua ^ date)` session hashing (`:4123–4134`), and its skip-lists.
3. **Parity window** (~2 weeks): `/earde-hq-dashboard` stays readable (data frozen
   at step 2) while PostHog Web Analytics is compared against it.
4. **Delete the dashboard**: route `bin/main.ml:199`, `hq_dashboard_handler`
   (`lib/handlers.ml:3807–3828`), `Pages.hq_dashboard_page`
   (`lib/pages.ml:4957–5081`), the admin-page link (`lib/pages.ml:4878`), and the
   now-unused DB functions: `log_page_view` (`lib/db.ml:2331–2339`),
   `get_kpi_dashboard` (`:2354–2372`), `get_dau_mau_ratio` (`:2379–2409`), their
   top-level aliases (`:3160–3163`, minus the presence touch) and `db.mli`
   declarations (`:415–417, 593–594`, minus the presence touch).
5. **Do NOT drop the `page_views` table** in the first migration/deployment. It
   stays as historical data. Any eventual drop is a separate forward-only
   migration requiring explicit human approval (per `CLAUDE.md`).

## 8. Configuration

Environment variables (read like the existing env pattern in `bin/main.ml:7–27` /
`lib/turnstile.ml`; `.env` stays untracked; **no real tokens committed anywhere,
including this document and example files**):

| Variable | Meaning | Production value shape |
|---|---|---|
| `POSTHOG_ENABLED` | master switch; when not `true`, no snippet is emitted, no server capture happens, no consent banner shows | `true` / `false` |
| `POSTHOG_PROJECT_TOKEN` | project API token (`phc_…`) | from PostHog project settings |
| `POSTHOG_API_HOST` | ingest host | `https://eu.i.posthog.com` |
| `POSTHOG_UI_HOST` | UI host (toolbar/links) and **private REST API host** for §3.3 | `https://eu.posthog.com` |
| `POSTHOG_PERSONAL_API_KEY` | **server-only secret** (`phx_…`): personal API key minimally scoped to person read/write on this project; used exclusively by the §3.3 deletion lifecycle | from PostHog personal API key settings |
| `POSTHOG_PROJECT_ID` | **server-only**, numeric project id for the §3.3 private REST API paths; **required when person deletion is enabled** (i.e. whenever `POSTHOG_PERSONAL_API_KEY` is set). Never hardcoded in `lib/analytics.ml`; never rendered into browser configuration — v1 has no browser-side need for it | current production value: `229260` |
| `EARDE_PUBLIC_ORIGIN` | the exact allowed `Origin` for `POST /analytics/consent` (§9); never hardcoded in the handler | `https://earde.com` |

Credential handling: `POSTHOG_PROJECT_TOKEN` is a public, write-only ingest
token and may be rendered to browsers. `POSTHOG_PERSONAL_API_KEY` is a secret —
it must never be rendered into HTML, shipped in any JavaScript, exposed through
any response, or logged; it is read from the environment only inside
`lib/analytics.ml`. If `POSTHOG_PERSONAL_API_KEY` or `POSTHOG_PROJECT_ID` is
unset, deletion attempts log a warning and skip the HTTP call — the §3.3 jobs
are still inserted and stay `pending`, so nothing is lost (and local deletion is
unaffected); startup should warn when only one of the two is set.

The browser receives the project token and hosts via escaped attributes rendered
by `layout` (§2.1). The Wasp landing repo uses the same project token and hosts,
supplied at build time through its `REACT_APP_*` client-env mechanism (§11); it
never needs — and must never receive — `POSTHOG_PERSONAL_API_KEY` or
`POSTHOG_PROJECT_ID`.

No CSP exists today (audit-confirmed: zero security headers), so no header change
is required. If a CSP is added later, it must allow the two PostHog hosts.

## 9. Consent and privacy controls

Mechanics only — user-facing copy and legal wording are a separate product task
(`CLAUDE.md`: analytics must be disclosed deliberately; `/privacy` update happens
with rollout but its text is not specified here).

State is a first-party cookie `earde_analytics_consent` with exactly these
attributes (part of the cross-repo contract, §1):

- value: exactly the enum `granted` | `denied` — no personal information;
- `Path=/` — so the Wasp landing shares it;
- `Secure` in production;
- `SameSite=Lax`;
- **`HttpOnly=false`** — deliberate and required: the prerendered landing HTML
  is identical for every visitor, so the landing can determine consent only by
  reading `document.cookie` after React hydration;
- ~180-day Max-Age.

The cookie is **set server-side only**, through a minimal same-origin endpoint;
`analytics.js` never writes it directly.

**`POST /analytics/consent`** (new route in `bin/main.ml`, handler
`analytics_consent_handler` in `lib/handlers.ml`):

This endpoint is deliberately **not** protected by the `Dream.csrf_tag`
mechanism used by every other state-changing route. That mechanism cannot work
here: its tokens exist only inside Dream-rendered HTML and are bound to a Dream
session, while the prerendered Wasp landing must call this endpoint on a
visitor's very first request — a first-time landing visitor has **no Dream
session** (their requests have only ever hit Nginx's static file serving) and
no Dream-rendered page to read a token from. The endpoint instead uses a
route-specific protection suited to its narrow, enum-only surface. **All other
Earde state-changing routes retain their existing CSRF mechanism unchanged.**

- Accepts only `Content-Type: application/json`; form-encoded submissions and
  every other content type are rejected with a controlled `400` and no cookie
  change.
- The JSON body must contain exactly one field:
  `state = "granted" | "denied"`. Any other value, a missing field, or any
  additional field → controlled `400`, no cookie change.
- Requires an exact `Origin` header match against the configured public origin
  `EARDE_PUBLIC_ORIGIN` (§8) — the allowed origin comes from configuration,
  never hardcoded in the handler.
- Checks `Sec-Fetch-Site` and accepts only `same-origin` or `same-site`
  browser requests.
- Missing or invalid origin metadata → controlled `403`, no cookie change.
- Requires **no existing Dream session**: consent must be recordable before
  Dream has ever served the visitor anything.
- On a valid value, sets `earde_analytics_consent` server-side with the
  attributes above (`Set-Cookie` from Dream), overwriting any previous value.
- Returns a minimal controlled response suitable for `fetch` (`204 No
  Content`, or a tiny JSON `{"ok": true}`); errors are controlled `400`/`403`
  JSON, never a rendered error page.
- Browser callers (Dream `analytics.js` and the landing module alike) send
  `fetch("/analytics/consent", { method: "POST", credentials: "same-origin",
  … })` with the JSON body.
- On `granted`, when the request carries an authenticated Dream session: after `state`
  has been validated as exactly `granted` and the origin checks above have
  passed, the handler
  loads the current user's analytics values (username, email, `created_at`,
  `is_admin`) and calls `sync_person_after_consent_grant` (§3.1). The
  request-inspecting `capture_if_consented` cannot be used here — the response
  `Set-Cookie` does not modify the current `Dream.request`, so at this moment
  the `granted` value exists only in the outgoing response — which is exactly
  why the narrowly scoped transition function exists. The §4.3 person
  properties become available immediately, with **no logout/login required**.
- On `denied`: sets the cookie and emits **no** analytics of any kind.

Browser sequencing in `analytics.js` (banner and settings toggle both go
through the endpoint):

- **Accept** → `POST /analytics/consent` with `state=granted`; **only after the
  endpoint succeeds** does `analytics.js` initialize PostHog (then identify and
  group context per §4.2/§5.3). If the endpoint fails, nothing initializes and
  the banner stays.
- **Refuse / revoke** → `POST /analytics/consent` with `state=denied`; **only
  after the endpoint succeeds** does `analytics.js` call
  `posthog.opt_out_capturing()` + `posthog.reset()` and clear PostHog
  cookies/localStorage (no-op if the SDK was never initialized).

The Wasp landing calls the same same-origin endpoint from its own banner (§11)
— which works on a first visit precisely because the protection above depends
on neither a Dream-rendered token nor a pre-existing Dream session — keeping
one consent state across `/` and the app. This requires the Nginx routing
prerequisite in §10.4.

| State | Browser behavior | Server behavior |
|---|---|---|
| **Before any choice** | SDK is **not initialized**: no PostHog cookies, no storage, no requests. A small consent banner (rendered by `layout` when `POSTHOG_ENABLED` and no cookie) is shown. | `capture_if_consented` (§3.1) finds no `granted` cookie → no capture. |
| **After acceptance** | Endpoint sets the `granted` cookie; SDK initializes only after the endpoint succeeds (and on every subsequent load); identify and group context run per §4.2/§5.3. | `capture_if_consented` proceeds for requests carrying `granted`; the consent handler immediately syncs person properties for a logged-in user (§4.3); `$groups` (§5.3) ride on subsequent captures. |
| **After refusal** | Endpoint sets the `denied` cookie; banner hides; SDK never loads. | No capture. |
| **After later revocation** | The settings toggle (`lib/pages.ml:4235` area) posts `state=denied`; only after the endpoint succeeds does `analytics.js` call `posthog.opt_out_capturing()` + `posthog.reset()` and clear PostHog cookies/localStorage. | Capture stops from the next request. Revocation does not itself delete already-collected data; account deletion (§3.3) does. |

Cross-device caveat: consent travels as a browser cookie, so a flow that spans
browsers/devices — notably the signup confirmation email link — may arrive
without it; the corresponding server event is then simply not captured (§3.2).

## 10. Testing and rollout

### 10.1 Tests

Per project convention, `dune test` stays DB-free; DB-backed assertions go behind
the existing `EARDE_TEST_DATABASE_URL` opt-in gate.

- **Pure tests** (`test/test_earde.ml`): URL sanitizer (`/confirm-email?token=abc`
  → `/confirm-email`; `/search?q=x&page=2` → `/search`; fragments stripped);
  capture payload JSON for each event variant (allowlist enforced by the closed
  variant type — a test per constructor); `user:<id>` distinct-ID formatting;
  consent-cookie parsing.
- **Consent-gate tests**: `capture_if_consented` with a missing or `denied`
  cookie produces no capture (asserted via the sink below); `granted` does.
  There is no exported way to capture without a request — enforced by the `.mli`.
- **Consent endpoint tests**: `POST /analytics/consent` accepts exactly
  `state="granted"` and `state="denied"` as a single-field JSON body and sets
  the cookie with `Path=/`, `SameSite=Lax`, `Secure` (under production
  config), **no `HttpOnly`**, and the documented lifetime; any other value,
  additional fields, a missing field, a form-encoded body, or a non-JSON
  content type → `400` with no cookie change; a missing or mismatching
  `Origin` (vs `EARDE_PUBLIC_ORIGIN`) or a cross-site `Sec-Fetch-Site` →
  `403` with no cookie change; a request with valid origin metadata but **no
  Dream session** still succeeds (the landing first-visit case); success
  returns the minimal fetch-friendly response; `granted` with an
  authenticated session triggers exactly one `sync_person_after_consent_grant`
  call (via the sink); `denied`, invalid-state, and origin-rejected requests
  trigger none. `sync_person_after_consent_grant` has no other call site than
  `analytics_consent_handler` (checked by review; the `.mli` documents the
  restriction).
- **Handler/event tests**: `lib/analytics.ml` exposes a test-only injectable sink
  so tests assert capture fires exactly once on the success arm and never on
  validation/permission failures — covering at minimum `comment_created` (with the
  new returned id) and `chat_message_sent` (both response modes).
- **Group and person-property payload tests** (pure): community-scoped event
  payloads carry `$groups.community = "community:<id>"`; `$groupidentify`
  payloads contain only the §5.3 allowlisted group properties; `$set` appears
  only on `signup_confirmed`/`login_succeeded` payloads and contains only the
  §4.3 person properties.
- **Deletion lifecycle tests** (job-table behavior behind the
  `EARDE_TEST_DATABASE_URL` gate where DB-backed): **atomicity** —
  `anonymize_user_and_enqueue_posthog_deletion` commits the anonymized user and
  the `pending` job together, and a forced mid-transaction failure rolls back
  both (user intact, no job) — there is no state with one but not the other; it
  returns the `(job_id, distinct_id)` used by `attempt_person_deletion`; with
  the private API stubbed as success/`202` the job becomes `completed` with
  `completed_at` set; stubbed as 4xx/5xx/unreachable the job stays `pending`
  with `attempts` incremented and `last_error` recorded — and local account
  deletion still succeeds; a missing `POSTHOG_PERSONAL_API_KEY` or
  `POSTHOG_PROJECT_ID` skips the HTTP call but still leaves the durable
  `pending` job; an already-absent person (empty UUID lookup) completes the job.
- **Retry operation tests**: `retry_pending_posthog_deletions` processes at most
  the fixed batch size, increments `attempts`, completes jobs whose person is
  accepted-or-absent, leaves failing jobs `pending`, and logs only job id,
  numeric user id, and HTTP status. Manual: after deleting a test account,
  verify the person disappears from the PostHog Persons page.
- **Forced PostHog failure**: run with `POSTHOG_API_HOST` pointing at a closed
  port and at a stub returning 500 — every product operation still succeeds, the
  response is unchanged, and no exception escapes the `Lwt.async` body.
- **Token URL sanitization**: unit tests above, plus a manual check that a real
  `/confirm-email?token=…` visit produces a Live Event with no token anywhere.
- **identify/reset verification** (manual, browser): anonymous browsing → login →
  the PostHog person `user:<id>` contains the pre-login pageviews and the §4.3
  person properties; logout → the distinct ID is a fresh anonymous one; deletion
  behaves like logout and additionally triggers §3.3.
- **Group-context verification** (manual, browser): visit a community page, then
  `/feed` — Live Events must show community-page events with the `community`
  group and `/feed` events with **no** group attached.
- **Replay masking verification** (manual): record a session covering live chat
  (including a message inserted by `chat_live.js`, not just SSR), a thread, search
  results, notifications, a private community, and the admin page; confirm every
  §6 target is masked/blocked while navigation and clicks remain visible.
- **Live Events verification**: after each rollout phase, confirm the expected
  events (and only those) in PostHog Live Events, with sanitized URLs.
- **Routing verification (deployment, §10.4)**: before enabling the landing
  consent banner, verify in production that a `POST /analytics/consent` issued
  from a page served at `/` reaches Dream (the cookie is set) and that `GET`
  or other unsupported methods receive a controlled response, not the landing
  HTML.
- **Landing CTA invariant (§11)**: verify the built landing HTML points every
  `ENTER EARDE` CTA at `https://earde.com/feed`.

### 10.2 File-by-file implementation map

| File | Change |
|---|---|
| `lib/analytics.ml` + `lib/analytics.mli` (new) | config from env (incl. `POSTHOG_PROJECT_ID` — never hardcoded); closed event variant + property serialization (incl. `$groups`, `$groupidentify`, `$set`); exported entry points: consent-gated `capture_if_consented`, the narrowly scoped `sync_person_after_consent_grant` (consent transition only, §3.1/§9), and job-scoped `attempt_person_deletion` (§3.3); the private-API deletion client; URL/consent helpers shared with tests |
| `lib/handlers.ml` | new `presence_middleware`; new `analytics_consent_handler` (JSON-only, `Origin`/`Sec-Fetch-Site`-checked, no-session-required — §9); switch `delete_account_handler` to `Db.anonymize_user_and_enqueue_posthog_deletion` + post-commit capture/attempt (§3.3); delete `analytics_middleware` (phase 2); capture calls at the nine success anchors (§3.2); delete `hq_dashboard_handler` (phase 4) |
| `lib/handlers.mli` | expose `presence_middleware` + `analytics_consent_handler`; drop `analytics_middleware` / `hq_dashboard_handler` when removed |
| `bin/main.ml` | env vars (incl. `EARDE_PUBLIC_ORIGIN`); wire `presence_middleware`; `POST /analytics/consent` route; entry point for the manual `retry_pending_posthog_deletions` maintenance command; unwire analytics middleware (phase 2); remove `/earde-hq-dashboard` route `:199` (phase 4) |
| `lib/components.ml` | snippet + config attrs + identity attr + consent banner in `layout` (`:320–335`); `ph-mask` on feed-card title/preview (`:799/804`); `ph-mask` on ban-reason textareas region |
| `lib/pages.ml` | `ph-mask` / `ph-no-capture` additions (§6); `data-analytics-group` on community-scoped shells (§5.3); search results `data-*` for `search_performed`; consent toggle on settings; remove `hq_dashboard_page` `:4957–5081` and admin link `:4878` (phase 4) |
| `lib/db.ml` + `lib/db.mli` | `create_comment` → `RETURNING id`; `pending_signup_confirm` returns user id; move `touch_user_active` to a presence-named home; transactional `anonymize_user_and_enqueue_posthog_deletion` (§3.3) + deletion-job functions (mark completed/failed, list pending batch); remove `log_page_view` / `get_kpi_dashboard` / `get_dau_mau_ratio` (phase 4) |
| `static/js/analytics.js` (new) | consent banner + settings toggle posting to `/analytics/consent` (single-field JSON body, `credentials: "same-origin"` — §9); SDK init only after endpoint success; manual sanitized pageviews; identify/reset; group/resetGroups; `search_performed` |
| `test/test_earde.ml` | pure tests + handler capture tests + consent endpoint, deletion-job, and retry-operation tests (§10.1) |
| `db/` | **one additive, forward-only migration**: `posthog_person_deletion_jobs` (§3.3). `page_views` and `users.last_active_at` untouched — no destructive change |
| `services/realtime_gateway/` | untouched |

### 10.3 Commit sequence (each: `dune build` && `dune test` green)

1. **Extract presence middleware** — behavior-neutral split of
   `analytics_middleware`; `last_active_at` writes proven unchanged.
2. **Add `lib/analytics.ml(i)` + pure tests** — env config (incl.
   `POSTHOG_PROJECT_ID`), consent-gated capture API,
   `sync_person_after_consent_grant`, `attempt_person_deletion`, payload
   builders (events, `$groups`, `$groupidentify`, `$set`), deletion client;
   compiles and is tested, not yet wired anywhere.
3. **DB-layer returns** — `create_comment RETURNING id`,
   `pending_signup_confirm` returns id; call sites adjusted; tests.
4. **Consent endpoint + browser SDK** — `POST /analytics/consent` route +
   handler (JSON-only validation, `Origin`/`Sec-Fetch-Site` checks against
   `EARDE_PUBLIC_ORIGIN`, server-set cookie,
   `sync_person_after_consent_grant` on granted) + layout snippet + config
   attrs + consent banner + `analytics.js` with init-after-endpoint
   sequencing, manual sanitized pageviews, and replay masking config; endpoint
   tests.
5. **Identity & group context (browser)** — identity + `data-analytics-group`
   attributes; identify/reset and group/resetGroups logic.
6. **Server events** — `capture_if_consented` calls at the nine success anchors,
   with `$groups`, `$groupidentify`, and person `$set`; handler tests;
   forced-failure check.
7. **Durable account-deletion lifecycle** — `posthog_person_deletion_jobs`
   migration + transactional `Db.anonymize_user_and_enqueue_posthog_deletion`
   + job functions; `delete_account_handler` switched to it, with post-commit
   `capture_if_consented` + `attempt_person_deletion ~job_id ~distinct_id`;
   `retry_pending_posthog_deletions` maintenance command; atomicity,
   stubbed-API, and retry tests.
8. **`search_performed`** — results-page data attributes + browser capture.
9. **Stop `page_views` writes** — delete the reduced `analytics_middleware`
   (after Live Events + Web Analytics verification).
10. **Remove the old dashboard** — route, handler, page, admin link, dead DB
    functions (after the parity window). `page_views` table stays.

Rollout gates between commits 8→9 (PostHog page-view data verified correct) and
9→10 (parity window elapsed) are deliberate pauses, not code steps.

### 10.4 Deployment prerequisites (Nginx)

Neither repository owns the production Nginx configuration, so this is recorded
as a **deployment prerequisite and rollout check**, not a code change:

- `POST /analytics/consent` must be proxied to the Dream application.
- That location must be evaluated **before** the landing's static-file serving
  and before any `200.html` SPA-fallback rule — the landing build emits a
  `200.html` fallback that would otherwise absorb the path and answer with
  landing HTML.
- `GET` and other unsupported methods on `/analytics/consent` should receive a
  controlled response (e.g. `405`), never the landing HTML.
- Verify this routing in production (§10.1 routing verification) **before**
  enabling the landing consent banner.

## 11. Landing repository integration (specified separately)

The landing-side implementation lives in the separate Wasp repository and will
get its own specification there; this document only fixes the cross-repo
contract (§1, §4.2, §8, §9, §10.4). That specification will cover:

- `src/env.ts` — adding the public client vars (PostHog project token and
  host as `REACT_APP_*` variables) to the zod validation schema;
- `.env.client.example` — the tracked example values (no real tokens);
- a new consent/PostHog client module or component — banner, consent-gated
  `posthog-js` initialization, the `/analytics/consent` fetch, the §4.2
  anonymous-side reset check, and group-context clearing;
- `LandingPage.tsx` — mounting the banner;
- CTA instrumentation — `cta_click` with a location property on the three
  `ENTER EARDE` buttons (header / hero / final), plus nav and demo events;
- `landing.css` — banner styles;
- the `posthog-js` dependency;
- prerender and hydration verification — the fully-styled no-JS page must be
  preserved, with the banner present but hidden until hydration.

No server-only value ever enters the landing repository:
`POSTHOG_PERSONAL_API_KEY` and `POSTHOG_PROJECT_ID` remain Dream-only (§8).
The landing needs only the public project token and hosts.

**Deployment invariant — CTA target**: the `ENTER EARDE` CTAs must point to
`https://earde.com/feed`. The landing's current untracked `.env.client` sets
`REACT_APP_EARDE_APP_URL=https://earde.com` (origin only), which disagrees with
the last build artifact (which baked `…/feed`); the value must be corrected to
the `/feed` URL during implementation, and the built HTML verified as part of
the landing rollout (§10.1).
