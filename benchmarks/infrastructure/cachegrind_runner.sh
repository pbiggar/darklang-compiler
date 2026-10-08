#!/bin/bash
# Run cachegrind benchmark for a given problem
# Usage: ./cachegrind_runner.sh <benchmark_name> <output_dir> [parity_status] [baseline_refresh] [dark_binary] [profile]
#
# By default, only measures Dark; the summary processor reads stored Rust JSON.
# Pass `rust` as baseline_refresh to re-run the audited Rust reference.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BENCHMARKS_DIR="$(dirname "$SCRIPT_DIR")"
BENCHMARK=$1
OUTPUT_DIR=$2
PARITY_STATUS=${3:-comparable}
REFRESH_BASELINE=${4:-false}
DARK_BINARY=${5:-}
PROFILE=${6:-full}
source "$SCRIPT_DIR/pretty.sh"

if [ -z "$BENCHMARK" ] || [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <benchmark_name> <output_dir>"
    exit 1
fi

PROBLEM_DIR="$BENCHMARKS_DIR/problems/$BENCHMARK"
DARK_BINARY="${DARK_BINARY:-$PROBLEM_DIR/dark/main}"
EXPECTED=$(python3 "$SCRIPT_DIR/benchmark_profiles.py" expected "$PROFILE" "$BENCHMARK")
# Regular files also work in minimal VMs without /dev/fd process substitution.
python3 "$SCRIPT_DIR/benchmark_profiles.py" arguments "$PROFILE" "$BENCHMARK" > "$OUTPUT_DIR/${BENCHMARK}_arguments.txt"
mapfile -t BENCHMARK_ARGUMENTS < "$OUTPUT_DIR/${BENCHMARK}_arguments.txt"

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
STARTED_RESULTS=false
FINALIZED_RESULTS=false

finalize_results_file() {
    if [ "$STARTED_RESULTS" = true ] && [ "$FINALIZED_RESULTS" = false ]; then
        echo "]}" >> "$RESULTS_FILE_PATH"
        FINALIZED_RESULTS=true
    fi
}

on_exit() {
    finalize_results_file
}

trap on_exit EXIT

# Create output file for parsed results
echo "{\"benchmark\": \"$BENCHMARK\", \"results\": [" > "$RESULTS_FILE_PATH"
STARTED_RESULTS=true

FIRST=true

# Dark always runs; the audited Rust reference runs only during an explicit
# baseline refresh.
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

        CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no --cachegrind-out-file=/dev/null "$BINARY" "${BENCHMARK_ARGUMENTS[@]}" 2>&1)

        I_REFS=$(printf '%s\n' "$CG_OUTPUT" | sed -n 's/.*I refs:[[:space:]]*//p' | tr -d ',')
        if [[ ! "$I_REFS" =~ ^[1-9][0-9]*$ ]]; then
            pretty_fail "$impl Cachegrind did not produce exactly one positive instruction count"
            exit 1
        fi

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
    "instructions": $I_REFS
  }
EOF

        pretty_info "Instructions: $I_REFS"
    fi
done

echo "]}" >> "$RESULTS_FILE_PATH"
FINALIZED_RESULTS=true
pretty_ok "Results saved to: $RESULTS_FILE_PATH"
