#!/bin/bash
# Build all implementations for a given benchmark
# Usage: ./build_all.sh <benchmark_name>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$BENCHMARKS_DIR")"
BENCHMARK=$1
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ]; then
    echo "Usage: $0 <benchmark_name>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"

if [ ! -d "$PROBLEM_DIR" ]; then
    echo "Error: Benchmark '$BENCHMARK' not found at $PROBLEM_DIR"
    exit 1
fi

pretty_section "Building $BENCHMARK..."

# Build Dark implementation
if [ -f "$PROBLEM_DIR/dark/main.dark" ]; then
    pretty_info "Building Dark..."
    "$PROJECT_ROOT/dark" "$PROBLEM_DIR/dark/main.dark" -o "$PROBLEM_DIR/dark/main" -q
    chmod +x "$PROBLEM_DIR/dark/main"
    pretty_ok "Dark build complete"
fi

RUN_BASELINES=${RUN_BASELINES:-true}
if [ "$RUN_BASELINES" != "true" ]; then
    pretty_info "Baseline builds skipped (set RUN_BASELINES=true or --refresh-baseline)"
    pretty_ok "Build complete."
    exit 0
fi

# Build Rust implementation
if [ -f "$PROBLEM_DIR/rust/main.rs" ]; then
    if command -v rustc &> /dev/null; then
        pretty_info "Building Rust..."
        rustc -C opt-level=3 "$PROBLEM_DIR/rust/main.rs" -o "$PROBLEM_DIR/rust/main" 2>/dev/null
        pretty_ok "Rust build complete"
    else
        pretty_warn "Rust skipped (rustc not installed)"
    fi
fi

# Build OCaml implementation
if [ -f "$PROBLEM_DIR/ocaml/main.ml" ]; then
    if command -v ocamlopt &> /dev/null; then
        pretty_info "Building OCaml..."
        if ocamlopt -O3 "$PROBLEM_DIR/ocaml/main.ml" -o "$PROBLEM_DIR/ocaml/main" 2>/dev/null; then
            pretty_ok "OCaml build complete"
        else
            pretty_warn "OCaml build failed"
        fi
    else
        pretty_warn "OCaml skipped (ocamlopt not installed)"
    fi
fi

# Build Go implementation
if [ -f "$PROBLEM_DIR/go/main.go" ]; then
    if command -v go &> /dev/null; then
        pretty_info "Building Go..."
        if go build -o "$PROBLEM_DIR/go/main" "$PROBLEM_DIR/go/main.go" 2>/dev/null; then
            pretty_ok "Go build complete"
        else
            pretty_warn "Go build failed"
        fi
    else
        pretty_warn "Go skipped (go not installed)"
    fi
fi

# F# scripts don't need building (run via dotnet fsi)
if [ -f "$PROBLEM_DIR/fsharp/main.fsx" ]; then
    pretty_info "F#: no build needed (script)"
fi

# Python doesn't need building
if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    pretty_info "Python: no build needed"
fi

# Node.js doesn't need building
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    pretty_info "Node.js: no build needed"
fi

# Bun uses same JS files as Node
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    pretty_info "Bun: no build needed (uses Node.js files)"
fi

pretty_ok "Build complete."
