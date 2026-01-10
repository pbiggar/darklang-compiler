#!/bin/bash
# Run cachegrind benchmark for a given problem
# Usage: ./cachegrind_runner.sh <benchmark_name> <output_dir>
#
# By default, only runs Dark and uses cached baselines from BASELINES.md.
# Set REFRESH_BASELINE=all to re-run all languages.
# Set REFRESH_BASELINE=rust,go,python to re-run specific languages.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
RESULTS_FILE="$BENCHMARKS_DIR/RESULTS.md"

# Check for valgrind
if ! command -v valgrind &> /dev/null; then
    pretty_fail "valgrind is not installed"
    pretty_info "Install with: sudo apt-get install valgrind"
    exit 1
fi

# Helper to check if a language should run
should_run_lang() {
    local lang="$1"
    if [ -z "$REFRESH_BASELINE" ] || [ "$REFRESH_BASELINE" = "false" ]; then
        return 1
    fi
    if [ "$REFRESH_BASELINE" = "true" ] || [ "$REFRESH_BASELINE" = "all" ]; then
        return 0
    fi
    echo ",$REFRESH_BASELINE," | grep -q ",$lang,"
}

latest_results_count() {
    local lang="$1"
    local column_index=""

    case "$lang" in
        dark) column_index=3 ;;
        rust) column_index=4 ;;
        ocaml) column_index=5 ;;
        python) column_index=6 ;;
        node) column_index=7 ;;
        *) return ;;
    esac

    if [ ! -f "$RESULTS_FILE" ]; then
        return
    fi

    awk -F'|' -v bench="$BENCHMARK" -v idx="$column_index" '
        function trim(s) { gsub(/^[ \t]+|[ \t]+$/, "", s); return s }
        /^\|/ {
            name = trim($2)
            if (name == bench) {
                value = trim($(idx))
                gsub(/\([^)]*\)/, "", value)
                value = trim(value)
                print value
                exit
            }
        }' "$RESULTS_FILE"
}

numeric_count() {
    echo "$1" | tr -d ',' | tr -d ' '
}

format_improvement() {
    local current="$1"
    local baseline="$2"

    if [ -z "$current" ] || [ -z "$baseline" ] || [ "$baseline" = "0" ]; then
        return
    fi

    awk -v current="$current" -v baseline="$baseline" 'BEGIN {
        diff = (baseline - current) / baseline * 100.0
        if (diff >= 0) {
            printf "+%.1f%% vs latest", diff
        } else {
            printf "%.1f%% vs latest", diff
        }
    }'
}

pretty_section "Running cachegrind benchmark for $BENCHMARK..."

RESULTS_FILE_PATH="$OUTPUT_DIR/${BENCHMARK}_cachegrind.json"
STARTED_RESULTS=false
FINALIZED_RESULTS=false

finalize_results_file() {
    if [ "$STARTED_RESULTS" = true ] && [ "$FINALIZED_RESULTS" = false ]; then
        echo "]}" >> "$RESULTS_FILE_PATH"
        FINALIZED_RESULTS=true
    fi
}

trap finalize_results_file EXIT

# Create output file for parsed results
echo "{\"benchmark\": \"$BENCHMARK\", \"results\": [" > "$RESULTS_FILE_PATH"
STARTED_RESULTS=true

FIRST=true

# Determine which implementations to run
# Dark always runs; compiled languages (rust, ocaml) run if selected
# Note: Go crashes under valgrind due to runtime stack management
IMPLS="dark"
for lang in rust ocaml; do
    if should_run_lang "$lang"; then
        IMPLS="$IMPLS $lang"
    fi
done

# Run cachegrind for each implementation
for impl in $IMPLS; do
    BINARY="$PROBLEM_DIR/$impl/main"
    if [ -x "$BINARY" ]; then
        pretty_info "Running cachegrind on $impl..."

        # Run cachegrind and capture stderr (where stats are printed)
        CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes "$BINARY" 2>&1)

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

        LATEST_RESULTS=$(latest_results_count "$impl")
        LATEST_NUM=$(numeric_count "$LATEST_RESULTS")
        CURRENT_NUM=$(numeric_count "$I_REFS")
        if [ -n "$LATEST_NUM" ] && [ "$LATEST_RESULTS" != "-" ] && [ "$LATEST_NUM" != "$CURRENT_NUM" ]; then
            IMPROVEMENT=$(format_improvement "$CURRENT_NUM" "$LATEST_NUM")
            if [ -n "$IMPROVEMENT" ]; then
                pretty_info "Instructions: $I_REFS (latest RESULTS.md: $LATEST_RESULTS, $IMPROVEMENT)"
            else
                pretty_info "Instructions: $I_REFS"
            fi
        else
            pretty_info "Instructions: $I_REFS"
        fi
    fi
done

# Handle Python separately (run via interpreter) - only if selected
# Python timeout: 5 minutes (300 seconds) - some benchmarks are too slow
PYTHON_TIMEOUT=${PYTHON_TIMEOUT:-300}

if should_run_lang "python" && [ -f "$PROBLEM_DIR/python/main.py" ]; then
    if command -v python3 &> /dev/null; then
        pretty_info "Running cachegrind on python (timeout: ${PYTHON_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        if CG_OUTPUT=$(timeout "$PYTHON_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes python3 "$PROBLEM_DIR/python/main.py" 2>&1); then
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

            LATEST_RESULTS=$(latest_results_count "python")
            LATEST_NUM=$(numeric_count "$LATEST_RESULTS")
            CURRENT_NUM=$(numeric_count "$I_REFS")
            if [ -n "$LATEST_NUM" ] && [ "$LATEST_RESULTS" != "-" ] && [ "$LATEST_NUM" != "$CURRENT_NUM" ]; then
                IMPROVEMENT=$(format_improvement "$CURRENT_NUM" "$LATEST_NUM")
                if [ -n "$IMPROVEMENT" ]; then
                    pretty_info "Instructions: $I_REFS (latest RESULTS.md: $LATEST_RESULTS, $IMPROVEMENT)"
                else
                    pretty_info "Instructions: $I_REFS"
                fi
            else
                pretty_info "Instructions: $I_REFS"
            fi
        fi
    fi
fi

# Handle Node.js separately (run via interpreter) - only if selected
NODE_TIMEOUT=${NODE_TIMEOUT:-300}

if should_run_lang "node" && [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v node &> /dev/null; then
        pretty_info "Running cachegrind on node (timeout: ${NODE_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        if CG_OUTPUT=$(timeout "$NODE_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes node "$PROBLEM_DIR/node/main.js" 2>&1); then
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

            LATEST_RESULTS=$(latest_results_count "node")
            LATEST_NUM=$(numeric_count "$LATEST_RESULTS")
            CURRENT_NUM=$(numeric_count "$I_REFS")
            if [ -n "$LATEST_NUM" ] && [ "$LATEST_RESULTS" != "-" ] && [ "$LATEST_NUM" != "$CURRENT_NUM" ]; then
                IMPROVEMENT=$(format_improvement "$CURRENT_NUM" "$LATEST_NUM")
                if [ -n "$IMPROVEMENT" ]; then
                    pretty_info "Instructions: $I_REFS (latest RESULTS.md: $LATEST_RESULTS, $IMPROVEMENT)"
                else
                    pretty_info "Instructions: $I_REFS"
                fi
            else
                pretty_info "Instructions: $I_REFS"
            fi
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
