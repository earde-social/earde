# Local development setup

These steps set up Earde on a Linux machine from a fresh clone. They need no
production credentials and no external accounts.

## Tested versions

These are the versions CI and the maintainers use. Other versions may work, but
these are the ones known to.

| Tool | Version | Notes |
|---|---|---|
| OCaml | 5.1.1 | via opam 2.1 or newer |
| dune | 3.21.1 | the version locked in `earde.opam.locked` |
| ocamlformat | 0.28.1 | in its own opam switch, see [Formatting](#formatting) |
| PostgreSQL | 16 | tested with 16.15 |
| dbmate | 2.31.0 | |
| libargon2 | Ubuntu 24.04 `libargon2-dev` | linked by the server |
| ImageMagick | 6.9.12 (7 also works) | converts uploaded images at runtime |
| Gleam | 1.17.0 | realtime gateway only |
| Erlang/OTP | 27.3.4.13 | the gateway needs OTP 27 or newer |
| rebar3 | 3.27.0 | builds one Erlang dependency of the gateway |

OCaml dependency versions are locked in `earde.opam.locked`, and the gateway's in
`services/realtime_gateway/manifest.toml`.

## 1. System packages

On Ubuntu 24.04:

```sh
sudo apt-get install build-essential pkg-config git curl unzip \
  libargon2-dev libpq-dev libgmp-dev libev-dev libssl-dev libffi-dev \
  imagemagick postgresql
```

Install opam from <https://opam.ocaml.org/doc/Install.html>. Install dbmate by
downloading its release binary:

```sh
curl -fsSL -o dbmate https://github.com/amacneil/dbmate/releases/download/v2.31.0/dbmate-linux-amd64
sudo install -m 0755 dbmate /usr/local/bin/dbmate
```

## 2. OCaml dependencies

From the repository root:

```sh
opam init --bare -y                  # first opam use on this machine only
opam switch create . 5.1.1 --no-install
eval $(opam env)
opam install . --deps-only --with-test --locked -y
dune build
```

`opam switch create .` makes a switch in `_opam/`, which is git-ignored.
`--locked` installs exactly the versions in `earde.opam.locked`.

## 3. Database

Create a role and a database for development:

```sh
sudo -u postgres createuser --createdb earde
sudo -u postgres psql -c "ALTER ROLE earde PASSWORD 'earde'"
```

Then configure the environment and apply every migration:

```sh
cp .env.example .env         # the defaults work with the role above
set -a; . ./.env; set +a
dbmate up                    # creates earde_dev and applies db/migrations
```

`dbmate up` also rewrites `db/schema.sql`. The migrations are the source of
truth, and `db/schema.sql` is kept only so schema changes are reviewable. Commit
it together with a new migration. `scripts/check-schema.sh` checks that they
agree, starting from an empty database.

## 4. Run the application

```sh
set -a; . ./.env; set +a
dune exec bin/main.exe
```

Open <http://localhost:8080>. The server must run from the repository root,
because it serves `static/` from the working directory.

To get an account:

1. With `EARDE_SIGNUPS_ENABLED=true` and `EARDE_LOG_TOKENS=1` (both set in
   `.env.example`), sign up at `/signup`.
2. The confirmation link appears in the server log; open it.
3. To make yourself a global administrator, which you need to create
   communities, run:

   ```sh
   psql "$DATABASE_URL" -c "UPDATE users SET is_admin = true WHERE username = 'your-name'"
   ```

## 5. Realtime gateway (optional)

Without the gateway, chat still works over HTTP; pages just do not update live.
To run it, uncomment the realtime variables in `.env`, restart the application,
and in a second terminal:

```sh
cd services/realtime_gateway
set -a; . ../../.env; set +a
gleam run                    # listens on 127.0.0.1:8090
```

`REALTIME_ALLOWED_ORIGINS=http://localhost:8080` is needed locally, because the
page and the gateway are on different ports.

## Tests

- `dune test` runs the DB-free suite. Cases that need a database are skipped.
- The gated cases need a separate, disposable database, migrated the same way:

  ```sh
  createdb earde_test
  DATABASE_URL=postgres://earde:earde@localhost:5432/earde_test?sslmode=disable dbmate --no-dump-schema up
  EARDE_TEST_DATABASE_URL=postgresql://earde:earde@localhost:5432/earde_test scripts/test-gated.sh
  ```

  `scripts/test-gated.sh` fails unless every registered case ran. Alcotest leaves
  skipped cases out of its count, so a count check is the only way to catch
  them.
- `scripts/check-schema.sh` rebuilds the schema on a database that does not
  exist yet and compares it with `db/schema.sql`:

  ```sh
  DATABASE_URL=postgres://earde:earde@localhost:5432/earde_schema_check?sslmode=disable scripts/check-schema.sh
  ```

- Gateway tests: `cd services/realtime_gateway && gleam test`.
  `scripts/check-crash-contract.sh` there checks that a subsystem crash ends
  `gleam run` with a non-zero status.

## Formatting

OCaml and dune files are formatted with ocamlformat 0.28.1 (pinned in
`.ocamlformat`). Installing ocamlformat into the application's switch would make
opam downgrade some of the application's dependencies. Keep it in a separate
switch:

```sh
opam switch create earde-fmt 5.1.1
opam install --switch=earde-fmt -y dune ocamlformat.0.28.1
opam exec --switch=earde-fmt -- dune build @fmt   # check
opam exec --switch=earde-fmt -- dune fmt          # apply
```

The gateway uses `gleam format` (`gleam format --check src test`).
