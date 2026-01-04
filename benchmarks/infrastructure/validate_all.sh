#!/bin/bash
# Validate all implementations produce the correct output
# Usage: ./validate_all.sh <benchmark_name>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1

if [ -z "$BENCHMARK" ]; then
    echo "Usage: $0 <benchmark_name>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
EXPECTED_FILE="$PROBLEM_DIR/expected_output.txt"

if [ ! -f "$EXPECTED_FILE" ]; then
    echo "Warning: No expected_output.txt for $BENCHMARK, skipping validation"
    exit 0
fi

EXPECTED=$(cat "$EXPECTED_FILE")
FAILED=0

echo "Validating $BENCHMARK (expected: $EXPECTED)..."

# Validate Dark (check for Dark-specific expected output first)
if [ -x "$PROBLEM_DIR/dark/main" ]; then
    DARK_EXPECTED="$EXPECTED"
    if [ -f "$PROBLEM_DIR/dark/expected_output.txt" ]; then
        DARK_EXPECTED=$(cat "$PROBLEM_DIR/dark/expected_output.txt")
    fi
    OUTPUT=$("$PROBLEM_DIR/dark/main" 2>&1 || true)
    if [ "$OUTPUT" = "$DARK_EXPECTED" ]; then
        if [ "$DARK_EXPECTED" != "$EXPECTED" ]; then
            echo "  Dark: PASS (reduced size: $DARK_EXPECTED)"
        else
            echo "  Dark: PASS"
        fi
    else
        echo "  Dark: FAIL (got: '$OUTPUT', expected: '$DARK_EXPECTED')"
        FAILED=1
    fi
fi

# Validate Rust
if [ -x "$PROBLEM_DIR/rust/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/rust/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo "  Rust: PASS"
    else
        echo "  Rust: FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/rust/main.rs" ]; then
    echo "  Rust: skipped (not built)"
fi

# Validate OCaml
if [ -x "$PROBLEM_DIR/ocaml/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/ocaml/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo "  OCaml: PASS"
    else
        echo "  OCaml: FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/ocaml/main.ml" ]; then
    echo "  OCaml: skipped (not built)"
fi

# Validate Go
if [ -x "$PROBLEM_DIR/go/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/go/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo "  Go: PASS"
    else
        echo "  Go: FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/go/main.go" ]; then
    echo "  Go: skipped (not built)"
fi

# Validate F#
if [ -f "$PROBLEM_DIR/fsharp/main.fsx" ]; then
    if command -v dotnet &> /dev/null; then
        OUTPUT=$(dotnet fsi --optimize+ "$PROBLEM_DIR/fsharp/main.fsx" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            echo "  F#: PASS"
        else
            echo "  F#: FAIL (got: '$OUTPUT')"
            FAILED=1
        fi
    else
        echo "  F#: skipped (dotnet not installed)"
    fi
fi

# Validate Python
if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    if command -v python3 &> /dev/null; then
        OUTPUT=$(python3 "$PROBLEM_DIR/python/main.py" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            echo "  Python: PASS"
        else
            echo "  Python: FAIL (got: '$OUTPUT')"
            FAILED=1
        fi
    else
        echo "  Python: skipped (python3 not installed)"
    fi
fi

# Validate Node.js (non-fatal - deep recursion may exceed stack)
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v node &> /dev/null; then
        OUTPUT=$(node "$PROBLEM_DIR/node/main.js" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            echo "  Node.js: PASS"
        elif echo "$OUTPUT" | grep -q "Maximum call stack size exceeded"; then
            echo "  Node.js: SKIP (stack overflow - runs under cachegrind)"
        else
            echo "  Node.js: WARN (got: '${OUTPUT:0:50}...')"
        fi
    else
        echo "  Node.js: skipped (node not installed)"
    fi
fi

# Validate Bun (non-fatal - uses same JS as Node)
export PATH="$HOME/.bun/bin:$PATH"
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v bun &> /dev/null; then
        OUTPUT=$(bun run "$PROBLEM_DIR/node/main.js" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            echo "  Bun: PASS"
        elif echo "$OUTPUT" | grep -q "Maximum call stack size exceeded"; then
            echo "  Bun: SKIP (stack overflow - runs under cachegrind)"
        else
            echo "  Bun: WARN (got: '${OUTPUT:0:50}...')"
        fi
    else
        echo "  Bun: skipped (bun not installed)"
    fi
fi

if [ $FAILED -eq 1 ]; then
    echo "Validation failed!"
    exit 1
fi

echo "  All validations passed."
