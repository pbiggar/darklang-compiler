#!/bin/bash
# Run hyperfine benchmark for a given problem
# Usage: ./hyperfine_runner.sh <benchmark_name> <output_dir> [parity_status] [dark_binary]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2
PARITY_STATUS=${3:-comparable}
DARK_BINARY=${4:-}
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
DARK_BINARY="${DARK_BINARY:-$PROBLEM_DIR/dark/main}"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    pretty_fail "hyperfine is not installed"
    pretty_info "Install with: brew install hyperfine (macOS) or cargo install hyperfine"
    exit 1
fi

# Build command list
COMMANDS=()

if [ -x "$DARK_BINARY" ]; then
    COMMANDS+=(-n "Dark" "$DARK_BINARY")
fi

if [ "$PARITY_STATUS" = "comparable" ] && [ -x "$PROBLEM_DIR/rust/main" ]; then
    COMMANDS+=(-n "Rust" "$PROBLEM_DIR/rust/main")
fi

if [ ${#COMMANDS[@]} -eq 0 ]; then
    pretty_fail "No implementations found for $BENCHMARK"
    exit 1
fi

pretty_section "Running hyperfine benchmark for $BENCHMARK..."

hyperfine \
    --warmup 3 \
    --runs 10 \
    --export-json "$OUTPUT_DIR/${BENCHMARK}_hyperfine.json" \
    --export-markdown "$OUTPUT_DIR/${BENCHMARK}_summary.md" \
    "${COMMANDS[@]}"
