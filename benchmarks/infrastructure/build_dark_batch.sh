#!/bin/bash
# build_dark_batch.sh - Compile benchmark programs with one reusable stdlib build.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$BENCHMARKS_DIR")"
COMPILER="$PROJECT_ROOT/dark"
OUTPUT_DIR=""
BENCHMARKS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
        --compiler=*) COMPILER="${1#*=}" ;;
        --output-dir=*) OUTPUT_DIR="${1#*=}" ;;
        --*)
            echo "Unknown option: $1" >&2
            exit 1
            ;;
        *) BENCHMARKS+=("$1") ;;
    esac
    shift
done

if [[ -z "$OUTPUT_DIR" || ${#BENCHMARKS[@]} -eq 0 ]]; then
    echo "Usage: $0 [--compiler=PATH] --output-dir=PATH BENCHMARK..." >&2
    exit 1
fi

if [[ ! -x "$COMPILER" ]]; then
    echo "Dark compiler is not executable: $COMPILER" >&2
    exit 1
fi

# Trusted benchmark inputs use the private positional-argument helper.
BATCH_ARGS=(--batch --quiet --allow-internal --)
OUTPUTS=()
for benchmark in "${BENCHMARKS[@]}"; do
    source_path="$BENCHMARKS_DIR/problems/$benchmark/dark/main.dark"
    output_path="$OUTPUT_DIR/$benchmark/dark/main"
    if [[ ! -f "$source_path" ]]; then
        echo "Dark benchmark source not found: $source_path" >&2
        exit 1
    fi
    mkdir -p "$(dirname "$output_path")"
    rm -f "$output_path"
    BATCH_ARGS+=("$source_path" "$output_path")
    OUTPUTS+=("$output_path")
done

if ! "$COMPILER" "${BATCH_ARGS[@]}"; then
    for output_path in "${OUTPUTS[@]}"; do
        rm -f "$output_path"
    done
    exit 1
fi

for output_path in "${OUTPUTS[@]}"; do
    chmod +x "$output_path"
done
