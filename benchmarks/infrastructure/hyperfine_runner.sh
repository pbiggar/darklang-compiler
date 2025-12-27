#!/bin/bash
# Run hyperfine benchmark for a given problem
# Usage: ./hyperfine_runner.sh <benchmark_name> <output_dir>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"

# Check for hyperfine
if ! command -v hyperfine &> /dev/null; then
    echo "Error: hyperfine is not installed"
    echo "Install with: brew install hyperfine (macOS) or cargo install hyperfine"
    exit 1
fi

# Build command list
COMMANDS=()

if [ -x "$PROBLEM_DIR/dark/main" ]; then
    COMMANDS+=(-n "Dark" "$PROBLEM_DIR/dark/main")
fi

if [ -x "$PROBLEM_DIR/rust/main" ]; then
    COMMANDS+=(-n "Rust" "$PROBLEM_DIR/rust/main")
fi

if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    COMMANDS+=(-n "Python" "python3 $PROBLEM_DIR/python/main.py")
fi

if [ ${#COMMANDS[@]} -eq 0 ]; then
    echo "Error: No implementations found for $BENCHMARK"
    exit 1
fi

echo "Running hyperfine benchmark for $BENCHMARK..."

hyperfine \
    --warmup 3 \
    --runs 10 \
    --export-json "$OUTPUT_DIR/${BENCHMARK}_hyperfine.json" \
    --export-markdown "$OUTPUT_DIR/${BENCHMARK}_summary.md" \
    "${COMMANDS[@]}"
