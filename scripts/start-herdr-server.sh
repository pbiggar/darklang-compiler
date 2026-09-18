#!/usr/bin/env bash
# start-herdr-server.sh - Idempotently start the container's persistent Herdr server.

set -euo pipefail

server_is_running() {
  herdr status server 2>/dev/null | grep -q '^status: running$'
}

if server_is_running; then
  exit 0
fi

nohup herdr server </dev/null >/dev/null 2>&1 &

for _attempt in {1..50}; do
  if server_is_running; then
    exit 0
  fi

  sleep 0.1
done

printf 'Herdr server did not become ready\n' >&2
exit 1
