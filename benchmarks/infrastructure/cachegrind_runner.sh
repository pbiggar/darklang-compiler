#!/bin/bash
# Run cachegrind benchmark for a given problem
# Usage: ./cachegrind_runner.sh <benchmark_name> <output_dir>
#
# By default, only runs Dark and uses cached Rust/Python baselines from HISTORY.md.
# Set REFRESH_BASELINE=true to re-run Rust and Python.

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

# Check for valgrind
if ! command -v valgrind &> /dev/null; then
    echo "Error: valgrind is not installed"
    echo "Install with: sudo apt-get install valgrind"
    exit 1
fi

echo "Running cachegrind benchmark for $BENCHMARK..."

# Create output file for parsed results
RESULTS_FILE="$OUTPUT_DIR/${BENCHMARK}_cachegrind.json"
echo "{\"benchmark\": \"$BENCHMARK\", \"results\": [" > "$RESULTS_FILE"

FIRST=true

# Determine which implementations to run
if [ "$REFRESH_BASELINE" = "true" ]; then
    IMPLS="dark rust"
else
    IMPLS="dark"
fi

# Run cachegrind for each implementation
for impl in $IMPLS; do
    BINARY="$PROBLEM_DIR/$impl/main"
    if [ -x "$BINARY" ]; then
        echo "  Running cachegrind on $impl..."

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
            echo "," >> "$RESULTS_FILE"
        fi

        # Write JSON entry
        cat >> "$RESULTS_FILE" << EOF
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

        echo "    Instructions: $I_REFS"
    fi
done

# Handle Python separately (run via interpreter) - only if refreshing baseline
# Python timeout: 5 minutes (300 seconds) - some benchmarks are too slow
PYTHON_TIMEOUT=${PYTHON_TIMEOUT:-300}

if [ "$REFRESH_BASELINE" = "true" ] && [ -f "$PROBLEM_DIR/python/main.py" ]; then
    if command -v python3 &> /dev/null; then
        echo "  Running cachegrind on python (timeout: ${PYTHON_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        if CG_OUTPUT=$(timeout "$PYTHON_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes python3 "$PROBLEM_DIR/python/main.py" 2>&1); then
            PYTHON_SUCCESS=true
        else
            EXIT_CODE=$?
            if [ $EXIT_CODE -eq 124 ]; then
                echo "    TIMEOUT: Python benchmark exceeded ${PYTHON_TIMEOUT}s, skipping"
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
                echo "," >> "$RESULTS_FILE"
            fi

            cat >> "$RESULTS_FILE" << EOF
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

            echo "    Instructions: $I_REFS"
        fi
    fi
fi

# Handle Node.js separately (run via interpreter) - only if refreshing baseline
NODE_TIMEOUT=${NODE_TIMEOUT:-300}

if [ "$REFRESH_BASELINE" = "true" ] && [ -f "$PROBLEM_DIR/node/main.js" ]; then
    if command -v node &> /dev/null; then
        echo "  Running cachegrind on node (timeout: ${NODE_TIMEOUT}s)..."

        # Use timeout to avoid hanging on slow benchmarks
        if CG_OUTPUT=$(timeout "$NODE_TIMEOUT" valgrind --tool=cachegrind --cache-sim=yes --branch-sim=yes node "$PROBLEM_DIR/node/main.js" 2>&1); then
            NODE_SUCCESS=true
        else
            EXIT_CODE=$?
            if [ $EXIT_CODE -eq 124 ]; then
                echo "    TIMEOUT: Node benchmark exceeded ${NODE_TIMEOUT}s, skipping"
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
                echo "," >> "$RESULTS_FILE"
            fi

            cat >> "$RESULTS_FILE" << EOF
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

            echo "    Instructions: $I_REFS"
        fi
    fi
fi

echo "]}" >> "$RESULTS_FILE"
echo "  Results saved to: $RESULTS_FILE"
