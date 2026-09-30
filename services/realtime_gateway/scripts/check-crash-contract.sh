#!/usr/bin/env bash
# The outer half of the gateway's recovery contract (see realtime_gateway.start):
# when an essential subsystem dies, `gleam run` must exit with a non-zero
# status so a service manager restarts it, and nothing may keep answering on
# the port meanwhile. The in-VM half (every essential process is linked to
# main) is covered by test/gateway_recovery_test.gleam.
#
# Usage (from services/realtime_gateway, port 8090 free):
#   scripts/check-crash-contract.sh
set -euo pipefail
cd "$(dirname "$0")/.."

node="crash_gateway_$$"
cookie="crash-contract-$$"
log=$(mktemp)
trap 'rm -f "$log"' EXIT

gleam build
REALTIME_TOKEN_SECRET=check REALTIME_INTERNAL_SECRET=check \
  ERL_FLAGS="-sname $node -setcookie $cookie" gleam run >"$log" 2>&1 &
gateway=$!

for _ in $(seq 1 100); do
  curl -fsS http://127.0.0.1:8090/health >/dev/null 2>&1 && break
  sleep 0.1
done
curl -fsS http://127.0.0.1:8090/health >/dev/null

escript scripts/crash-contract.escript "$node@$(hostname -s)" "$cookie"

for _ in $(seq 1 100); do
  kill -0 "$gateway" 2>/dev/null || break
  sleep 0.1
done
if kill -0 "$gateway" 2>/dev/null; then
  kill "$gateway"
  echo "The gateway kept running after an essential subsystem died." >&2
  exit 1
fi
status=0
wait "$gateway" || status=$?
if [ "$status" -eq 0 ]; then
  echo "The gateway exited with status 0; a service manager would not restart it." >&2
  exit 1
fi
if curl -fsS -m 1 http://127.0.0.1:8090/health >/dev/null 2>&1; then
  echo "Something still answers on port 8090." >&2
  exit 1
fi
echo "The gateway exited with status $status after an essential subsystem died."
