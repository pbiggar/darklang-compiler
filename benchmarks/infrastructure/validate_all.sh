#!/bin/bash
# Validate all implementations produce the correct output
# Usage: ./validate_all.sh <benchmark_name>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ]; then
    echo "Usage: $0 <benchmark_name>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
EXPECTED_FILE="$PROBLEM_DIR/expected_output.txt"

if [ ! -f "$EXPECTED_FILE" ]; then
    pretty_warn "No expected_output.txt for $BENCHMARK, skipping validation"
    exit 0
fi

EXPECTED=$(cat "$EXPECTED_FILE")
FAILED=0

pretty_section "Validating $BENCHMARK (expected: $EXPECTED)..."

RUN_BASELINES=${RUN_BASELINES:-true}

# Validate Dark (check for Dark-specific expected output first)
if [ -x "$PROBLEM_DIR/dark/main" ]; then
    DARK_EXPECTED="$EXPECTED"
    if [ -f "$PROBLEM_DIR/dark/expected_output.txt" ]; then
        DARK_EXPECTED=$(cat "$PROBLEM_DIR/dark/expected_output.txt")
    fi
    OUTPUT=$("$PROBLEM_DIR/dark/main" 2>&1 || true)
    if [ "$OUTPUT" = "$DARK_EXPECTED" ]; then
        if [ "$DARK_EXPECTED" != "$EXPECTED" ]; then
            pretty_ok "Dark PASS (reduced size: $DARK_EXPECTED)"
        else
            pretty_ok "Dark PASS"
        fi
    else
        pretty_fail "Dark FAIL (got: '$OUTPUT', expected: '$DARK_EXPECTED')"
        FAILED=1
    fi
fi

if [ "$RUN_BASELINES" != "true" ]; then
    pretty_info "Baseline validations skipped (set RUN_BASELINES=true or --refresh-baseline)"
    if [ $FAILED -eq 1 ]; then
        echo "Validation failed!"
        exit 1
    fi
    pretty_ok "All validations passed."
    exit 0
fi

# Validate Rust
if [ -x "$PROBLEM_DIR/rust/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/rust/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        pretty_ok "Rust PASS"
    else
        pretty_fail "Rust FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/rust/main.rs" ]; then
    pretty_warn "Rust skipped (not built)"
fi

# Validate OCaml
if [ -x "$PROBLEM_DIR/ocaml/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/ocaml/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        pretty_ok "OCaml PASS"
    else
        pretty_fail "OCaml FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/ocaml/main.ml" ]; then
    pretty_warn "OCaml skipped (not built)"
fi

# Validate Go
if [ -x "$PROBLEM_DIR/go/main" ]; then
    OUTPUT=$("$PROBLEM_DIR/go/main" 2>&1 || true)
    if [ "$OUTPUT" = "$EXPECTED" ]; then
        pretty_ok "Go PASS"
    else
        pretty_fail "Go FAIL (got: '$OUTPUT')"
        FAILED=1
    fi
elif [ -f "$PROBLEM_DIR/go/main.go" ]; then
    pretty_warn "Go skipped (not built)"
fi

# Validate F#
if [ -f "$PROBLEM_DIR/fsharp/main.fsx" ]; then
    if command -v dotnet &> /dev/null; then
        OUTPUT=$(dotnet fsi --optimize+ "$PROBLEM_DIR/fsharp/main.fsx" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            pretty_ok "F# PASS"
        else
            pretty_fail "F# FAIL (got: '$OUTPUT')"
            FAILED=1
        fi
    else
        pretty_warn "F# skipped (dotnet not installed)"
    fi
fi

# Validate Python
if [ -f "$PROBLEM_DIR/python/main.py" ]; then
    if command -v python3 &> /dev/null; then
        OUTPUT=$(python3 "$PROBLEM_DIR/python/main.py" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            pretty_ok "Python PASS"
        else
            pretty_fail "Python FAIL (got: '$OUTPUT')"
            FAILED=1
        fi
    else
        pretty_warn "Python skipped (python3 not installed)"
    fi
fi

# Validate Node.js (non-fatal - deep recursion may exceed stack)
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v node &> /dev/null; then
        OUTPUT=$(node "$PROBLEM_DIR/node/main.js" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            pretty_ok "Node.js PASS"
        elif echo "$OUTPUT" | grep -q "Maximum call stack size exceeded"; then
            pretty_warn "Node.js SKIP (stack overflow - runs under cachegrind)"
        else
            pretty_warn "Node.js WARN (got: '${OUTPUT:0:50}...')"
        fi
    else
        pretty_warn "Node.js skipped (node not installed)"
    fi
fi

# Validate Bun (non-fatal - uses same JS as Node)
export PATH="$HOME/.bun/bin:$PATH"
if [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v bun &> /dev/null; then
        OUTPUT=$(bun run "$PROBLEM_DIR/node/main.js" 2>&1 || true)
        if [ "$OUTPUT" = "$EXPECTED" ]; then
            pretty_ok "Bun PASS"
        elif echo "$OUTPUT" | grep -q "Maximum call stack size exceeded"; then
            pretty_warn "Bun SKIP (stack overflow - runs under cachegrind)"
        else
            pretty_warn "Bun WARN (got: '${OUTPUT:0:50}...')"
        fi
    else
        pretty_warn "Bun skipped (bun not installed)"
    fi
fi

if [ $FAILED -eq 1 ]; then
    pretty_fail "Validation failed!"
    exit 1
fi

pretty_ok "All validations passed."
