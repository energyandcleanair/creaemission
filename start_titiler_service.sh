#!/bin/bash

# Start TiTiler service
# This script downloads and runs TiTiler directly

set -e

# Set default port if not provided (internal-only)
TITILER_PORT=${TITILER_PORT:-8001}
TITILER_TILESIZE=${TITILER_TILESIZE:-1024}

echo "Starting TiTiler on port $TITILER_PORT with tilesize $TITILER_TILESIZE"

# Check if venv exists and has titiler
if [ ! -f "/opt/titiler-venv/bin/python" ]; then
    echo "ERROR: TiTiler venv not found at /opt/titiler-venv/bin/python"
    exit 1
fi

echo "Starting TiTiler (uvicorn) on port $TITILER_PORT..."

# Prefer using venv's uvicorn directly
UVICORN_BIN="/opt/titiler-venv/bin/uvicorn"
if [ ! -x "$UVICORN_BIN" ]; then
  echo "ERROR: uvicorn binary not found at $UVICORN_BIN"
  exit 1
fi

# Start TiTiler application
exec "$UVICORN_BIN" titiler.application.main:app \
  --host 0.0.0.0 \
  --port "$TITILER_PORT" \
  --log-level info \
  --proxy-headers \
  --forwarded-allow-ips "*"
