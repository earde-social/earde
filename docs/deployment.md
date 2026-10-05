# Running Earde in production

This is a generic outline, not a turnkey recipe. It describes what a deployment
needs; adapt it to your own hosts and tooling.

## Components

- **Application:** `earde_server` (`dune build`, then run `_build/default/bin/main.exe`)
  - listens on port 8080, on `HOST` (default `localhost`);
  - serves `static/` from its working directory.
- **PostgreSQL 16**, migrated with `dbmate up`.
- **Realtime gateway** (optional): `gleam run` in `services/realtime_gateway`.
  It listens on 127.0.0.1:8090. The application reaches it at
  `REALTIME_GATEWAY_URL`, which must be a plain `http://` loopback URL.
- **Reverse proxy** in front of both, terminating TLS.

## Configuration

Copy `.env.example` and set at least:
- `DATABASE_URL`;
- a stable `DREAM_SECRET`, or sessions end at every restart;
- `BASE_URL`, and `EARDE_PUBLIC_ORIGIN` as an `https://` origin;
- `EARDE_TRUSTED_PROXIES`: the address of the proxy that connects to the
  application, and nothing else (default `127.0.0.1,::1`).

Everything else is optional and off by default: mail, Turnstile, the realtime
gateway, GitHub onboarding and analytics.

Keep secrets out of the repository and out of logs. Never set
`EARDE_LOG_TOKENS=1` on a shared host: it prints account-verification and
password-reset links.

## Reverse proxy

- Proxy everything to the application, and `/socket/` to the gateway, with
  WebSocket upgrade headers.
- Never expose the gateway's `/internal/publish` route.
- Set `REALTIME_ALLOWED_ORIGINS` on the gateway to your public origin.
- The stylesheet is split into partials loaded through `@import`. Serve
  `/static/` with caching headers, or let the proxy serve it directly, so
  browsers do not fetch every partial on every page.

### Client addresses

The per-client rate limits on login, signup, password-reset requests and the
other sensitive forms count attempts per client address. When the request comes
from a proxy in `EARDE_TRUSTED_PROXIES`, Earde takes that address from the
right-most entry of the last `X-Forwarded-For` header line; otherwise it uses
the TCP peer.

So the proxy that connects to Earde must make sure that value is the client
address it determined itself. If it passes the client's header through, the
client chooses the value Earde reads, and with it the bucket its attempts are
counted in. Appending works only while exactly one proxy appends to exactly one
header line; replacing the header holds in every topology, so use it. With nginx
as the only proxy:

```nginx
proxy_set_header X-Forwarded-For $remote_addr;
```

If other proxies or a CDN sit in front of nginx, `$remote_addr` is the previous
hop, not the client. Resolve the real client address at the edge, have each
later hop accept it only from the hop before it (in nginx, the `realip` module's
`set_real_ip_from` and `real_ip_header`), and still pass a single address to
Earde as above. Whatever the topology, the value Earde receives must be one that
no client can set.

## Services

Run each process under a service manager that restarts it on failure, for
example systemd with `Restart=always`.

The gateway relies on this. If one of its essential subsystems crashes, the
whole gateway exits with a non-zero status rather than keep running degraded, and
clients reconnect and catch up over HTTP once it is back (see
`services/realtime_gateway/README.md`).

The gateway needs Erlang/OTP 27 or newer on the service's `PATH`. `gleam run`
does not pass SIGTERM on to the Erlang VM it starts. Under systemd's default
`KillMode=control-group` the whole group is stopped, which is what you want; with
a service manager that signals only the main process, run the Erlang shipment's
`entrypoint.sh` (`gleam export erlang-shipment`) instead, or an old gateway can
outlive its stop and keep port 8090.

## Upgrades

Some migrations change the schema in ways the previous code cannot use, so an
upgrade never lets old code serve against a migrated database. The order:

1. **Build and verify the target revision.** Check out the exact commit and
   confirm it with `git rev-parse HEAD`. Build it (`dune build`, and
   `gleam build` in `services/realtime_gateway`) and make sure its CI run passed.
   Do this in a separate checkout or build directory: if the running service
   starts from the same tree it builds in, a restart before step 4 would run the
   new code against the old schema. List what it will apply with
   `dbmate status`, run against the production database: these are the pending
   migrations.
2. **Stop every writer of the old version.** Stop all application processes,
   the gateway, and any job that writes to the database (for example
   `retry_posthog_deletions`). Then confirm nothing old is still connected:
   ```sql
   SELECT pid, application_name, client_addr, state
     FROM pg_stat_activity
    WHERE datname = current_database() AND pid <> pg_backend_pid();
   ```
3. **Make a recovery checkpoint and verify it.** Dump the database (`pg_dump
   -Fc`) and copy `static/uploads/`. Taking it after step 2 means it holds every
   committed write. Verify it before going on: restore the dump into a scratch
   database and compare `SELECT version FROM schema_migrations` and a few row
   counts with production.
4. **Apply the migrations:** `dbmate --no-dump-schema up` (without the flag,
   dbmate rewrites the checked-in `db/schema.sql`). Then `dbmate status` must
   show none pending.
5. **Start the new version:** the gateway, then the application, both built
   from the revision in step 1.
6. **Smoke-test and record.** Load `/feed`, log in, post a thread and a chat
   message (live delivery and the page reload should both show it), and check
   the server log for errors. Record the running commit and the applied
   migrations (`dbmate status`).

## If an upgrade goes wrong

Recovery is forward-only:

- **Preferred:** fix forward. Deploy a corrected revision that works with the
  migrated schema, using the same procedure.
- **Otherwise:** restore the verified checkpoint from step 3 as a whole (the
  database and `static/uploads/` together) and run the revision that matches
  it. Everything written after the checkpoint is lost, so treat this as a
  decision, not a routine step.

Never run an older revision against a database that has been migrated past it,
and do not run `dbmate down` on production data. The down migrations are kept
for development only.

An example of why. Migration `20260929130000_add_github_verification_freshness`
lets a project release its claim on a GitHub repository: it replaces the unique
constraint on `project_repositories.github_repository_id` with a unique index
over unreleased claims only. Code from before that migration (before commit
`95ed3ec`, PR #47) creates projects with `ON CONFLICT (github_repository_id)`,
which needs the old constraint:

- while old code runs against the migrated schema, every project creation fails;
- once the new code has released a claim and another project has claimed the
  same repository, the old constraint can no longer be restored, and old code,
  which ignores `released_at`, would treat both claims as active.

Back up `static/uploads/` together with the database: uploaded images are files,
not rows.
