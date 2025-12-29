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
        rustc -C opt-level=3 "$PROBLEM_DIR/rust/main.rs" -o "$PROBLEM_DIR/rust/main" 2>/dev/null
    else
        echo "  Rust: skipped (rustc not installed)"
    fi
fi

# Build OCaml implementation
if [ -f "$PROBLEM_DIR/ocaml/main.ml" ]; then
    if command -v ocamlopt &> /dev/null; then
        echo "  Building OCaml..."
        ocamlopt -O3 "$PROBLEM_DIR/ocaml/main.ml" -o "$PROBLEM_DIR/ocaml/main" 2>/dev/null || echo "    OCaml build failed"
    else
        echo "  OCaml: skipped (ocamlopt not installed)"
    fi
fi

# Build Go implementation
if [ -f "$PROBLEM_DIR/go/main.go" ]; then
    if command -v go &> /dev/null; then
        echo "  Building Go..."
        go build -o "$PROBLEM_DIR/go/main" "$PROBLEM_DIR/go/main.go" 2>/dev/null || echo "    Go build failed"
    else
        echo "  Go: skipped (go not installed)"
    fi
fi

# F# scripts don't need building (run via dotnet fsi)
if [ -f "$PROBLEM_DIR/fsharp/main.fsx" ]; then
    echo "  F#: no build needed (script)"
fi

# Python doesn't need building
if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    echo "  Python: no build needed"
fi

# Node.js doesn't need building
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    echo "  Node.js: no build needed"
fi

# Bun uses same JS files as Node
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    echo "  Bun: no build needed (uses Node.js files)"
fi

echo "  Build complete."
