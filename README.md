# Earde

Earde is a community platform for technical communities: open-source projects,
developer groups, the people who maintain a tool and the people who use it. It
combines live text chat with durable forum threads and a server-rendered,
searchable archive, so a useful answer given in chat can become a thread that
people find later instead of scrolling away.

A community has chat channels, threads organised in sections, moderators, and
public or private visibility. A chat message can be turned into a thread that
links back to it. Communities can connect to each other and share a thread into
another community without copying it. An open-source project can prove through
GitHub that it controls a repository and get a community home for it.

## Status

This is an early open-source release. The code has been through several rounds
of review and hardening, but it has not been operated at scale, and the schema
still changes between releases. Read [Current limitations](#current-limitations) before you
deploy it.

## How it is built, and why

- **Server-rendered HTML** from one OCaml application built on
  [Dream](https://github.com/aantron/dream). Pages work before any JavaScript
  runs, public archive pages are plain documents that search engines can crawl,
  and there is one place where authorization is decided.
- **Small, page-scoped JavaScript and plain CSS.** There is no frontend
  framework and no JavaScript or CSS build step. Scripts add live chat and a few
  conveniences to pages that already work without them.
- **PostgreSQL is the only source of truth** for users, communities, chat
  messages, threads and moderation state. The schema is defined by the
  migrations in `db/migrations`, applied with
  [dbmate](https://github.com/amacneil/dbmate). Permissions, bans and
  moderation are enforced in the SQL that reads and writes the data.
- **A separate realtime gateway** (`services/realtime_gateway`, Gleam on Beryl
  and Mist) only fans out live messages, presence, typing and cursors over
  WebSockets. Dream commits a message first and then publishes it; if the
  gateway is down or slow, nothing is lost, and clients catch up over HTTP. The
  gateway stores nothing, so it can restart at any time.

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
opam exec --switch=earde-fmt -- dune build @fmt   # ocamlformat 0.28.1, see docs/setup.md
cd services/realtime_gateway && gleam test
```

CI (`.github/workflows/ci.yml`) runs all of these on every pull request. The
gated runner fails if any registered case was skipped.

## Current limitations

- **One application server.** Earde runs as a single process with a single,
  unclustered gateway. A gateway restart drops live connections; clients
  reconnect and catch up.
- **Upgrades stop the service.** Migrations are applied with the old version
  stopped, and recovery is forward-only. Follow
  [docs/deployment.md](docs/deployment.md).
- **Large archives.** Listings and search compute scores and comment counts for
  every candidate post, and search is substring matching without a full-text
  index. Pages beyond 1,000 are refused. See [SECURITY.md](SECURITY.md) for the
  measured cost and what to put in front of it.
- **Mail** is sent only through Brevo, from a fixed `noreply@earde.com` sender.
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
- [docs/deployment.md](docs/deployment.md): running and upgrading Earde in production
- [docs/features/](docs/features): design notes for specific features
- [CONTRIBUTING.md](CONTRIBUTING.md) and [SECURITY.md](SECURITY.md)

## How this project is developed

This software is developed with assistance from AI coding agents, with humans providing the ideas and leading the testing and debugging. We say this openly because it shaped how the project was built. If you are not happy with AI-developed code, this software is not for you. We are thankful to the great Antirez because the AI development workflow adopted in this project is largely based on his ideas about the thoughtful use of coding agents and software development in the AI era.

## License

Earde's source code is released under the [MIT License](LICENSE). Third-party
material and dependency licences are listed in
[THIRD_PARTY_NOTICES.md](THIRD_PARTY_NOTICES.md). The Earde name and logos are
not covered by the MIT License.
