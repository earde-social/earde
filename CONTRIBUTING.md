# Contributing to Earde

Thanks for your interest. Earde is a small project, so please open an issue to
discuss anything larger than a focused fix before you start. That way we can
agree on the approach first.

Security problems go through [SECURITY.md](SECURITY.md), not public issues.

## Development environment

Follow [docs/setup.md](docs/setup.md). [docs/architecture.md](docs/architecture.md)
explains where things live.

## Ground rules

The project's constraints are written down in [CLAUDE.md](CLAUDE.md), the
engineering guide for human and AI contributors alike. In short:

- **Server-rendered HTML** with small, page-scoped vanilla JavaScript and plain
  CSS. No SPA, client-side routing, npm or CSS/JS build chain.
- **PostgreSQL through Dream is the source of truth** for all durable data,
  authentication, authorization and moderation. The realtime gateway only fans
  out.
- **Rendering goes through `Html`**: text with `Html.text`, URLs with the policy
  for their context, markup only as string literals. See
  [docs/features/safe-rendering.md](docs/features/safe-rendering.md).
- **Keep interfaces current.** Every `lib/*.ml` has an `.mli`; keep it in step
  and expose only what other modules need.
- **Migrations are forward-only.** Add a new timestamped migration and never edit
  an applied one. Commit the regenerated `db/schema.sql` with it.
- **No new dependencies or services** without discussing them first.
- **Styles.** Shared route chrome lives in `static/css/routes/chrome.css`, and
  route-specific rules in the matching `static/css/routes/*.css`, scoped under
  the page's body class.

## Before opening a pull request

Run the checks CI runs:

```sh
dune build @install @check
dune test
EARDE_TEST_DATABASE_URL=... scripts/test-gated.sh     # needs a migrated test database
opam exec --switch=earde-fmt -- dune build @fmt
cd services/realtime_gateway && gleam format --check src test && gleam test
```

Add tests with logic changes. Put DB-free cases wherever possible, and use gated
cases (skipped without a database) for SQL, transactions and handlers wired to
the database.

Keep pull requests focused. Describe what changed and why, and how you verified
it.

## License

By contributing, you agree that your contributions are licensed under the
[MIT License](LICENSE), the same licence as the project.
