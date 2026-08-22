#!/bin/bash
# Run cachegrind benchmark for a given problem
# Usage: ./cachegrind_runner.sh <benchmark_name> <output_dir> <run_artifacts_dir> [parity_status] [baseline_refresh]
#
# By default, only runs Dark and uses the cached Rust row from BASELINES.md.
# Pass `rust` as baseline_refresh to re-run the audited Rust reference.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2
RUN_ARTIFACTS=$3
PARITY_STATUS=${4:-comparable}
REFRESH_BASELINE=${5:-false}
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ] || [ -z "$RUN_ARTIFACTS" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir> <run_artifacts_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
EXPECTED_FILE="$PROBLEM_DIR/expected_output.txt"
EXPECTED=""
HAS_EXPECTED=false

if [ -f "$EXPECTED_FILE" ]; then
    EXPECTED=$(cat "$EXPECTED_FILE")
    HAS_EXPECTED=true
else
    pretty_warn "No expected_output.txt for $BENCHMARK, skipping output validation"
fi

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

expected_output_for_impl() {
    local impl="$1"
    if [ "$impl" = "dark" ] && [ -f "$PROBLEM_DIR/dark/expected_output.txt" ]; then
        cat "$PROBLEM_DIR/dark/expected_output.txt"
    else
        echo "$EXPECTED"
    fi
}

verify_output() {
    local impl="$1"
    shift

    if [ "$HAS_EXPECTED" != "true" ]; then
        return 0
    fi

    local expected
    expected=$(expected_output_for_impl "$impl")
    local output
    output=$("$@" 2>&1 || true)

    if [ "$output" = "$expected" ]; then
        if [ "$impl" = "dark" ] && [ "$expected" != "$EXPECTED" ]; then
            pretty_ok "$impl output OK (reduced size)"
        else
            pretty_ok "$impl output OK"
        fi
        return 0
    fi

    pretty_fail "$impl output mismatch (got: '$output', expected: '$expected')"
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
    BINARY="$RUN_ARTIFACTS/$BENCHMARK/$impl/main"
    MANIFEST="$RUN_ARTIFACTS/$BENCHMARK/$impl/provenance.json"
    if [ -x "$BINARY" ]; then
        SOURCE="$PROBLEM_DIR/$impl/main.$([ "$impl" = dark ] && echo dark || echo rs)"
        AUDITED_SHA=$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["benchmarks"][sys.argv[2]][sys.argv[3] + "_sha256"])' "$BENCHMARKS_DIR/PARITY.json" "$BENCHMARK" "$impl")
        python3 "$SCRIPT_DIR/artifact_provenance.py" verify --benchmark "$BENCHMARK" --language "$impl" --source "$SOURCE" --executable "$BINARY" --manifest "$MANIFEST" --audited-source-sha256 "$AUDITED_SHA"
        PROVENANCE=$(cat "$MANIFEST")
        verify_output "$impl" "$BINARY"
        pretty_info "Running cachegrind on $impl..."

        # Run cachegrind and capture stderr (where stats are printed)
        CACHEGRIND_OUT_FILE="$CACHEGRIND_OUT_DIR/${BENCHMARK}_${impl}.out"
        rm -f "$CACHEGRIND_OUT_FILE"
        CACHEGRIND_OUT_FILES+=("$CACHEGRIND_OUT_FILE")
        CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" "$BINARY" 2>&1)

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
    "ll_misses": $LL_MISSES,
    "provenance": $PROVENANCE
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
        if CG_OUTPUT=$(timeout "$PYTHON_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" python3 "$PROBLEM_DIR/python/main.py" 2>&1); then
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
        if CG_OUTPUT=$(timeout "$NODE_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes --cachegrind-out-file="$CACHEGRIND_OUT_FILE" node "$PROBLEM_DIR/node/main.js" 2>&1); then
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
