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

# Validate Dark
if [ -x "$PROBLEM_DIR/dark/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/dark/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        echo "  Dark: PASS"
    else
        echo "  Dark: FAIL (got: '$OUTPUT')"
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

if [ $FAILED -eq 1 ]; then
    echo "Validation failed!"
    exit 1
fi

echo "  All validations passed."
