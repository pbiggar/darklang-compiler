#!/bin/bash
# Build all implementations for a given benchmark
# Usage: ./build_all.sh <benchmark_name>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$BENCHMARKS_DIR")"
BENCHMARK=$1

if [ -z "$BENCHMARK" ]; then
    echo "Usage: $0 <benchmark_name>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"

if [ ! -d "$PROBLEM_DIR" ]; then
    echo "Error: Benchmark '$BENCHMARK' not found at $PROBLEM_DIR"
    exit 1
fi

echo "Building $BENCHMARK..."

# Build Dark implementation
if [ -f "$PROBLEM_DIR/dark/main.dark" ]; then
    echo "  Building Dark..."
    "$PROJECT_ROOT/dark" "$PROBLEM_DIR/dark/main.dark" -o "$PROBLEM_DIR/dark/main" -q
    chmod +x "$PROBLEM_DIR/dark/main"
fi

# Build Rust implementation
if [ -f "$PROBLEM_DIR/rust/main.rs" ]; then
    if command -v rustc &> /dev/null; then
        echo "  Building Rust..."
        rustc -O "$PROBLEM_DIR/rust/main.rs" -o "$PROBLEM_DIR/rust/main" 2>/dev/null
    else
        echo "  Rust: skipped (rustc not installed)"
    fi
fi

# Python doesn't need building
if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    echo "  Python: no build needed"
fi

echo "  Build complete."
