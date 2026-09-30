#!/usr/bin/env bash
# Rebuild the schema from every tracked migration and compare it with
# db/schema.sql. The migrations are the source of truth; db/schema.sql is the
# pg_dump of a database migrated from empty, kept for review and diffing.
#
# Usage: DATABASE_URL=postgres://.../<new database name> scripts/check-schema.sh
# The database must not exist yet: the script creates it, so the comparison
# always starts from an empty database. It is left in place afterwards.
set -euo pipefail

: "${DATABASE_URL:?set DATABASE_URL to a database name that does not exist yet}"
cd "$(dirname "$0")/.."

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

dbmate create
dbmate --migrations-dir db/migrations --schema-file "$tmp/schema.sql" up

# Only pg_dump's version banner and its \restrict guard lines (added by
# newer 16.x clients) may differ between machines.
normalize() {
  grep -vE '^(-- Dumped (from database|by pg_dump) version |\\(un)?restrict )' "$1"
}

if diff -u <(normalize db/schema.sql) <(normalize "$tmp/schema.sql"); then
  echo "db/schema.sql matches $(ls db/migrations | wc -l) migrations applied to an empty database."
else
  echo "db/schema.sql differs from the migrated schema (diff above)." >&2
  echo "Regenerate it with: dbmate --schema-file db/schema.sql dump" >&2
  exit 1
fi
