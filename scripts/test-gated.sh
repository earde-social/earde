#!/usr/bin/env bash
# Run the full test suite, including the cases gated on a database, and fail
# unless every registered case ran. Gated cases skip without a database, and
# Alcotest leaves skipped cases out of its "tests run" count, so the count is
# compared with the registered inventory.
#
# Usage: EARDE_TEST_DATABASE_URL=postgresql://.../<migrated test database> \
#          scripts/test-gated.sh
# Use a disposable database migrated with dbmate: the cases create and delete
# their own rows.
set -euo pipefail

: "${EARDE_TEST_DATABASE_URL:?set EARDE_TEST_DATABASE_URL to a migrated test database}"
cd "$(dirname "$0")/.."

dune build @install test/test_earde.exe
registered=$(./_build/default/test/test_earde.exe list | grep -cE '^\S+ +[0-9]+ ')

log=$(mktemp)
trap 'rm -f "$log"' EXIT
dune test --force 2>&1 | tee "$log"

ran=$(sed -nE 's/.* ([0-9]+) tests? run\..*/\1/p' "$log" | tail -1)
if [ "$ran" != "$registered" ]; then
  echo "Expected all $registered registered cases to run, but ${ran:-no} ran." >&2
  exit 1
fi
echo "All $registered registered cases ran, none skipped."
