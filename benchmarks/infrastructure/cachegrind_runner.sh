#!/bin/bash
# Run cachegrind benchmark for a given problem
# Usage: ./cachegrind_runner.sh <benchmark_name> <output_dir> [parity_status] [baseline_refresh] [dark_binary] [profile]
#
# By default, only runs Dark and uses the cached Rust row from BASELINES.md.
# Pass `rust` as baseline_refresh to re-run the audited Rust reference.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2
PARITY_STATUS=${3:-comparable}
REFRESH_BASELINE=${4:-false}
DARK_BINARY=${5:-}
PROFILE=${6:-routine}
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
DARK_BINARY="${DARK_BINARY:-$PROBLEM_DIR/dark/main}"
EXPECTED=$(python3 "$SCRIPT_DIR/benchmark_profiles.py" expected "$PROFILE" "$BENCHMARK")
mapfile -t BENCHMARK_ARGUMENTS < <(python3 "$SCRIPT_DIR/benchmark_profiles.py" arguments "$PROFILE" "$BENCHMARK")

# Check for valgrind
if ! command -v valgrind &> /dev/null; then
    pretty_fail "valgrind is not installed"
    pretty_info "Install with: sudo apt-get install valgrind"
    exit 1
fi

# Helper to check if a language should run
should_run_lang() {
    local lang="$1"
    if [ "$lang" != "rust" ]; then
        return 1
    fi
    if [ -z "$REFRESH_BASELINE" ] || [ "$REFRESH_BASELINE" = "false" ]; then
        return 1
    fi
    echo ",$REFRESH_BASELINE," | grep -q ",$lang,"
}

verify_output() {
    local impl="$1"
    shift
    local output
    output=$("$@" "${BENCHMARK_ARGUMENTS[@]}" 2>&1 || true)

    if [ "$output" = "$EXPECTED" ]; then
        pretty_ok "$impl output OK"
        return 0
    fi

    pretty_fail "$impl output mismatch (got: '$output', expected: '$EXPECTED')"
    return 1
}

pretty_section "Running cachegrind benchmark for $BENCHMARK..."

RESULTS_FILE_PATH="$OUTPUT_DIR/${BENCHMARK}_cachegrind.json"
CACHEGRIND_OUT_DIR="$OUTPUT_DIR/cachegrind"
mkdir -p "$CACHEGRIND_OUT_DIR"
CACHEGRIND_OUT_FILES=()
STARTED_RESULTS=false
FINALIZED_RESULTS=false

finalize_results_file() {
    if [ "$STARTED_RESULTS" = true ] && [ "$FINALIZED_RESULTS" = false ]; then
        echo "]}" >> "$RESULTS_FILE_PATH"
        FINALIZED_RESULTS=true
    fi
}

clean_cachegrind_files() {
    local file
    for file in "${CACHEGRIND_OUT_FILES[@]}"; do
        rm -f "$file"
    done
}

on_exit() {
    finalize_results_file
    clean_cachegrind_files
}

trap on_exit EXIT

# Create output file for parsed results
echo "{\"benchmark\": \"$BENCHMARK\", \"results\": [" > "$RESULTS_FILE_PATH"
STARTED_RESULTS=true

FIRST=true

# Determine which implementations to run
# Dark always runs; compiled languages (rust, ocaml) run if selected
# Note: Go crashes under valgrind due to runtime stack management
IMPLS="dark"
if [ "$PARITY_STATUS" = "comparable" ]; then
    for lang in rust; do
        if should_run_lang "$lang"; then
            IMPLS="$IMPLS $lang"
        fi
    done
elif [ "$REFRESH_BASELINE" != "false" ]; then
    pretty_warn "$BENCHMARK is $PARITY_STATUS; reference-language comparisons skipped"
fi

# Run cachegrind for each implementation
for impl in $IMPLS; do
    if [ "$impl" = "dark" ]; then
        BINARY="$DARK_BINARY"
    else
        BINARY="$PROBLEM_DIR/$impl/main"
    fi
    if [ -x "$BINARY" ]; then
        verify_output "$impl" "$BINARY"
        pretty_info "Running cachegrind on $impl..."

        # Run cachegrind and capture stderr (where stats are printed)
        CACHEGRIND_OUT_FILE="$CACHEGRIND_OUT_DIR/${BENCHMARK}_${impl}.out"
        rm -f "$CACHEGRIND_OUT_FILE"
        CACHEGRIND_OUT_FILES+=("$CACHEGRIND_OUT_FILE")
        CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" "$BINARY" "${BENCHMARK_ARGUMENTS[@]}" 2>&1)

        # Parse the output
        I_REFS=$(echo "$CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')
        D_REFS=$(echo "$CG_OUTPUT" | grep "D refs:" | sed 's/.*D refs:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
        BRANCHES=$(echo "$CG_OUTPUT" | grep "Branches:" | sed 's/.*Branches:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
        MISPREDICTS=$(echo "$CG_OUTPUT" | grep "Mispredicts:" | sed 's/.*Mispredicts:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
        I1_MISSES=$(echo "$CG_OUTPUT" | grep "I1  misses:" | sed 's/.*I1  misses:[[:space:]]*//' | tr -d ',')
        D1_MISSES=$(echo "$CG_OUTPUT" | grep "D1  misses:" | sed 's/.*D1  misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
        LL_MISSES=$(echo "$CG_OUTPUT" | grep "LL misses:" | head -1 | sed 's/.*LL misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')

        # Add comma separator if not first
        if [ "$FIRST" = true ]; then
            FIRST=false
        else
            echo "," >> "$RESULTS_FILE_PATH"
        fi

        # Write JSON entry
        cat >> "$RESULTS_FILE_PATH" << EOF
  {
    "language": "$impl",
    "instructions": $I_REFS,
    "data_refs": $D_REFS,
    "branches": $BRANCHES,
    "branch_mispredicts": $MISPREDICTS,
    "i1_misses": $I1_MISSES,
    "d1_misses": $D1_MISSES,
    "ll_misses": $LL_MISSES
  }
EOF

        pretty_info "Instructions: $I_REFS"
    fi
done

# Handle Python separately (run via interpreter) - only if selected
# Python timeout: 5 minutes (300 seconds) - some benchmarks are too slow
PYTHON_TIMEOUT=300

if [ "$PARITY_STATUS" = "comparable" ] && should_run_lang "python" && [ -f "$PROBLEM_DIR/python/main.py" ]; then
    if command -v python3 &> /dev/null; then
        verify_output "python" python3 "$PROBLEM_DIR/python/main.py"
        pretty_info "Running cachegrind on python (timeout: ${PYTHON_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        CACHEGRIND_OUT_FILE="$CACHEGRIND_OUT_DIR/${BENCHMARK}_python.out"
        rm -f "$CACHEGRIND_OUT_FILE"
        CACHEGRIND_OUT_FILES+=("$CACHEGRIND_OUT_FILE")
        if CG_OUTPUT=$(timeout "$PYTHON_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" python3 "$PROBLEM_DIR/python/main.py" "${BENCHMARK_ARGUMENTS[@]}" 2>&1); then
            PYTHON_SUCCESS=true
        else
            EXIT_CODE=$?
            if [ $EXIT_CODE -eq 124 ]; then
                pretty_warn "Python timeout exceeded ${PYTHON_TIMEOUT}s, skipping"
                PYTHON_SUCCESS=false
            else
                # Other error - still try to parse output
                PYTHON_SUCCESS=true
            fi
        fi

        if [ "$PYTHON_SUCCESS" = true ]; then
            I_REFS=$(echo "$CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')
            D_REFS=$(echo "$CG_OUTPUT" | grep "D refs:" | sed 's/.*D refs:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            BRANCHES=$(echo "$CG_OUTPUT" | grep "Branches:" | sed 's/.*Branches:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            MISPREDICTS=$(echo "$CG_OUTPUT" | grep "Mispredicts:" | sed 's/.*Mispredicts:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            I1_MISSES=$(echo "$CG_OUTPUT" | grep "I1  misses:" | sed 's/.*I1  misses:[[:space:]]*//' | tr -d ',')
            D1_MISSES=$(echo "$CG_OUTPUT" | grep "D1  misses:" | sed 's/.*D1  misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            LL_MISSES=$(echo "$CG_OUTPUT" | grep "LL misses:" | head -1 | sed 's/.*LL misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')

            if [ "$FIRST" = true ]; then
                FIRST=false
            else
                echo "," >> "$RESULTS_FILE_PATH"
            fi

            cat >> "$RESULTS_FILE_PATH" << EOF
  {
    "language": "python",
    "instructions": $I_REFS,
    "data_refs": $D_REFS,
    "branches": $BRANCHES,
    "branch_mispredicts": $MISPREDICTS,
    "i1_misses": $I1_MISSES,
    "d1_misses": $D1_MISSES,
    "ll_misses": $LL_MISSES
  }
EOF

            pretty_info "Instructions: $I_REFS"
        fi
    fi
fi

# Handle Node.js separately (run via interpreter) - only if selected
NODE_TIMEOUT=${NODE_TIMEOUT:-300}

if should_run_lang "node" && [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v node &> /dev/null; then
        verify_output "node" node "$PROBLEM_DIR/node/main.js"
        pretty_info "Running cachegrind on node (timeout: ${NODE_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        CACHEGRIND_OUT_FILE="$CACHEGRIND_OUT_DIR/${BENCHMARK}_node.out"
        rm -f "$CACHEGRIND_OUT_FILE"
        CACHEGRIND_OUT_FILES+=("$CACHEGRIND_OUT_FILE")
        if CG_OUTPUT=$(timeout "$NODE_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" node "$PROBLEM_DIR/node/main.js" "${BENCHMARK_ARGUMENTS[@]}" 2>&1); then
            NODE_SUCCESS=true
        else
            EXIT_CODE=$?
            if [ $EXIT_CODE -eq 124 ]; then
                pretty_warn "Node timeout exceeded ${NODE_TIMEOUT}s, skipping"
                NODE_SUCCESS=false
            else
                # Other error - still try to parse output
                NODE_SUCCESS=true
            fi
        fi

        if [ "$NODE_SUCCESS" = true ]; then
            I_REFS=$(echo "$CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')
            D_REFS=$(echo "$CG_OUTPUT" | grep "D refs:" | sed 's/.*D refs:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            BRANCHES=$(echo "$CG_OUTPUT" | grep "Branches:" | sed 's/.*Branches:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            MISPREDICTS=$(echo "$CG_OUTPUT" | grep "Mispredicts:" | sed 's/.*Mispredicts:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            I1_MISSES=$(echo "$CG_OUTPUT" | grep "I1  misses:" | sed 's/.*I1  misses:[[:space:]]*//' | tr -d ',')
            D1_MISSES=$(echo "$CG_OUTPUT" | grep "D1  misses:" | sed 's/.*D1  misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')
            LL_MISSES=$(echo "$CG_OUTPUT" | grep "LL misses:" | head -1 | sed 's/.*LL misses:[[:space:]]*//' | sed 's/ .*//' | tr -d ',')

            if [ "$FIRST" = true ]; then
                FIRST=false
            else
                echo "," >> "$RESULTS_FILE_PATH"
            fi

            cat >> "$RESULTS_FILE_PATH" << EOF
  {
    "language": "node",
    "instructions": $I_REFS,
    "data_refs": $D_REFS,
    "branches": $BRANCHES,
    "branch_mispredicts": $MISPREDICTS,
    "i1_misses": $I1_MISSES,
    "d1_misses": $D1_MISSES,
    "ll_misses": $LL_MISSES
  }
EOF

            pretty_info "Instructions: $I_REFS"
        fi
    fi
fi

# F# is NOT supported with cachegrind - .NET runtime doesn't work properly under valgrind
# (GC initialization fails, and AOT binaries don't get accurate instruction counts)

# Go is NOT supported with cachegrind - runtime crashes under valgrind
# (SIGSEGV in runtime.getproccount during initialization - Go's stack management conflicts with valgrind)

# Bun is NOT supported with cachegrind - JIT-compiled code isn't properly instrumented
# (All benchmarks show ~2.17M instructions regardless of complexity - just measuring startup)

echo "]}" >> "$RESULTS_FILE_PATH"
FINALIZED_RESULTS=true
pretty_ok "Results saved to: $RESULTS_FILE_PATH"
