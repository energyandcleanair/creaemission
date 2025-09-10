#!/usr/bin/env bash
set -euo pipefail

SHINY_URL="${SHINY_URL:-http://127.0.0.1:3838/}"
TITILER_URL="${TITILER_INTERNAL_URL:-http://127.0.0.1:8000/}"
MAX_WAIT="${MAX_WAIT:-60}"

echo "[wait] Waiting for Shiny at ${SHINY_URL} and TiTiler at ${TITILER_URL} (timeout: ${MAX_WAIT}s)"

deadline=$(( $(date +%s) + MAX_WAIT ))

wait_for() {
  local url=$1
  while true; do
    if curl -fsS -o /dev/null "$url"; then
      return 0
    fi
    if [ "$(date +%s)" -ge "$deadline" ]; then
      echo "[wait] Timeout waiting for $url" >&2
      return 1
    fi
    sleep 1
  done
}

wait_for "$SHINY_URL"
wait_for "$TITILER_URL"

echo "[wait] Backends ready. Starting nginx..."
exec /usr/sbin/nginx -g "daemon off;"

