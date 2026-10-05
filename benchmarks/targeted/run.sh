#!/bin/bash
# run.sh - build and run compiler-focused targeted benchmark suites.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$(dirname "$SCRIPT_DIR")")"

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <json|integer128|all> <output-path>" >&2
    exit 1
fi

cd "$PROJECT_ROOT"
"$PROJECT_ROOT/build" --quiet

dotnet run \
    --project "$SCRIPT_DIR/TargetedBenchmarks.fsproj" \
    --configuration Release \
    -- "$1" "$2"
