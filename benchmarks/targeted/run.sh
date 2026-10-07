#!/usr/bin/env bash
# run.sh - Build the compiler and measure targeted workloads with Python.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"
"$PROJECT_ROOT/build" --quiet
exec python3 "$SCRIPT_DIR/measure.py" "$@"
