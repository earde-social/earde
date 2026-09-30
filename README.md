# Earde

Earde is a community platform for technical communities. It combines live text
chat with durable, structured forum threads and a server-rendered, searchable
archive, so discussion worth keeping can be found later instead of scrolling
away. Open-source projects can connect a GitHub repository and get a community
home.

## Architecture

- **Web application:** OCaml with [Dream](https://github.com/aantron/dream).
  Every page is server-rendered HTML that works before any JavaScript runs.
- **Browser code:** small, page-scoped vanilla JavaScript and plain CSS. There is
  no frontend framework and no JavaScript or CSS build step.
- **Database:** PostgreSQL is the only source of truth for users, communities,
  chat messages, threads and moderation state. The schema is defined by the
  migrations in `db/migrations` (applied with [dbmate](https://github.com/amacneil/dbmate)).
- **Realtime gateway** (`services/realtime_gateway`): a small Gleam service,
  built on Beryl and Mist, that fans out live messages, presence, typing and
  cursors over WebSockets.
  - Dream validates and commits a message first, then publishes it to the
    gateway.
  - If the gateway is down, nothing is lost: clients catch up over HTTP.

More detail: [docs/architecture.md](docs/architecture.md).

## Quick start

You need OCaml 5.1.1 (opam), PostgreSQL 16, dbmate, libargon2 and ImageMagick. The
gateway additionally needs Gleam 1.17 and Erlang/OTP 27.
[docs/setup.md](docs/setup.md) has tested versions and step-by-step
instructions. In short:

```sh
opam switch create . 5.1.1 --no-install
opam install . --deps-only --with-test --locked
cp .env.example .env && set -a && . ./.env && set +a   # edit DATABASE_URL first
dbmate up                                              # create and migrate the database
dune exec bin/main.exe                                 # http://localhost:8080
```

No external accounts are needed locally. Mail links are printed in the server log,
and GitHub onboarding, analytics and bot protection are off unless configured.

## Tests

```sh
dune build @install @check      # warnings are errors
dune test                       # DB-free suite
EARDE_TEST_DATABASE_URL=postgresql://.../earde_test scripts/test-gated.sh
                                # full suite on a migrated, disposable database
scripts/check-schema.sh         # db/schema.sql matches the migrations
dune build @fmt                 # ocamlformat 0.28.1, see docs/setup.md
cd services/realtime_gateway && gleam test
```

CI (`.github/workflows/ci.yml`) runs all of these on every pull request.

## Current limitations

- **Deployment.** One application server and one in-memory gateway process. The
  gateway is not clustered, and a gateway restart drops live connections; clients
  reconnect and catch up.
- **Mail.** It is sent only through Brevo, from a fixed `noreply@earde.com`
  sender.
- **GitHub onboarding** needs a GitHub App of your own. See
  [docs/features/github-verification-lifecycle.md](docs/features/github-verification-lifecycle.md).
- **Product analytics** (PostHog, consent-gated) is bound to the `earde.com`
  production origin in code.
- **Community creation.** Generic creation is limited to global administrators.
  Other users create a community home by connecting a GitHub project.
- **Language.** The interface is English only.

## Documentation

- [docs/setup.md](docs/setup.md): local development environment
- [docs/architecture.md](docs/architecture.md): code layout and request flow
- [docs/deployment.md](docs/deployment.md): running Earde in production
- [docs/features/](docs/features): design notes for specific features
- [CONTRIBUTING.md](CONTRIBUTING.md) and [SECURITY.md](SECURITY.md)

## License

Earde's source code is released under the [MIT License](LICENSE). Third-party
material and dependency licences are listed in
[THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md). The Earde name and logos are
not covered by the MIT License.
