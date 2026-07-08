#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [--hyperfine] [--refresh-baseline[=lang1,lang2]] [--jobs[=N]] [benchmark_name|all]
#
# Options:
#   --help                   Show this help message and exit
#   --hyperfine              Use hyperfine for timing (default: cachegrind for instruction counts)
#   --refresh-baseline       Re-run all baseline languages (default: use cached values)
#   --refresh-baseline=LANGS Re-run specific languages only (comma-separated: rust,go,python,node,ocaml)
#   --jobs, --jobs=N         Run up to N benchmarks in parallel (default: 1)
#   --list                   Print the benchmarks that would run and exit

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"

show_help() {
    sed -n '/^# Usage:/,/^$/ {
        s/^# \{0,1\}//
        p
    }' "$0"
}

# Parse options
USE_CACHEGRIND=true
export REFRESH_BASELINE=false
BENCHMARK="all"
BUILD_FAILURES=()
RUN_FAILURES=()
PROCESS_FAILURES=()
LIST_ONLY=false
JOB_COUNT=""
SKIP_BENCHMARKS=("quicksort")

while [[ $# -gt 0 ]]; do
    case $1 in
        --help|-h)
            show_help
            exit 0
            ;;
        --hyperfine)
            USE_CACHEGRIND=false
            shift
            ;;
        --refresh-baseline)
            export REFRESH_BASELINE="all"
            shift
            ;;
        --refresh-baseline=*)
            export REFRESH_BASELINE="${1#*=}"
            shift
            ;;
        --jobs)
            if [ -z "${2:-}" ]; then
                pretty_fail "--jobs requires a value"
                exit 1
            fi
            JOB_COUNT="$2"
            shift 2
            ;;
        --jobs=*)
            JOB_COUNT="${1#*=}"
            shift
            ;;
        --list)
            LIST_ONLY=true
            shift
            ;;
        *)
            BENCHMARK="$1"
            shift
            ;;
    esac
done

OUTPUT_DIR="$SCRIPT_DIR/results/$(date +%Y-%m-%d_%H%M%S)"
mkdir -p "$OUTPUT_DIR"

# Record compiler version
pretty_info "Recording compiler version..."
git -C "$PROJECT_ROOT" rev-parse HEAD > "$OUTPUT_DIR/compiler_version.txt"
git -C "$PROJECT_ROOT" log -1 --format="%s" >> "$OUTPUT_DIR/compiler_version.txt"

# Get list of benchmarks to run
if [ "$BENCHMARK" = "all" ]; then
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/ 2>/dev/null | xargs -n1 basename)
else
    BENCHMARKS="$BENCHMARK"
fi

should_skip() {
    local candidate="$1"
    for skip in "${SKIP_BENCHMARKS[@]}"; do
        if [ "$candidate" = "$skip" ]; then
            return 0
        fi
    done
    return 1
}

FILTERED_BENCHMARKS=()
SKIPPED_BENCHMARKS=()
for bench in $BENCHMARKS; do
    if should_skip "$bench"; then
        SKIPPED_BENCHMARKS+=("$bench")
    else
        FILTERED_BENCHMARKS+=("$bench")
    fi
done
BENCHMARKS="${FILTERED_BENCHMARKS[*]}"

if [ "$LIST_ONLY" = true ]; then
    for bench in "${FILTERED_BENCHMARKS[@]}"; do
        echo "$bench"
    done
    exit 0
fi

if [ -z "$JOB_COUNT" ]; then
    if [ -n "${BENCHMARK_JOBS:-}" ]; then
        JOB_COUNT="$BENCHMARK_JOBS"
    else
        JOB_COUNT=1
    fi
fi

case "$JOB_COUNT" in
    ''|*[!0-9]*)
        pretty_fail "Invalid job count: $JOB_COUNT"
        exit 1
        ;;
esac

if [ "$JOB_COUNT" -lt 1 ]; then
    pretty_fail "Job count must be at least 1"
    exit 1
fi

STATUS_DIR="$OUTPUT_DIR/status"
mkdir -p "$STATUS_DIR"

if [ "$USE_CACHEGRIND" = true ]; then
    if [ "$REFRESH_BASELINE" = "false" ]; then
        export RUN_BASELINES=false
        pretty_section "Mode: Cachegrind (instruction counts) - Dark only (use --refresh-baseline for baselines)"
    elif [ "$REFRESH_BASELINE" = "all" ]; then
        export RUN_BASELINES=true
        pretty_section "Mode: Cachegrind (instruction counts) - refreshing all baselines"
    else
        export RUN_BASELINES=true
        pretty_section "Mode: Cachegrind (instruction counts) - refreshing: $REFRESH_BASELINE"
    fi
else
    export RUN_BASELINES=true
    pretty_section "Mode: Hyperfine (timing)"
fi
pretty_info "Benchmarks to run: $BENCHMARKS"
pretty_info "Parallel jobs: $JOB_COUNT"
if [ "${#SKIPPED_BENCHMARKS[@]}" -ne 0 ]; then
    pretty_warn "Skipping benchmarks: ${SKIPPED_BENCHMARKS[*]}"
fi
echo ""

JOB_PIDS=()

run_benchmark_job() {
    local bench="$1"
    local status_file="$STATUS_DIR/${bench}.status"
    : > "$status_file"

    pretty_header "Benchmark: $bench"

    # Build all implementations
    if ! "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench"; then
        echo "BUILD_FAIL" >> "$status_file"
        pretty_warn "Build failed for $bench (continuing)"
    fi

    # Run benchmark
    if [ "$USE_CACHEGRIND" = true ]; then
        if ! "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR"; then
            echo "RUN_FAIL" >> "$status_file"
            pretty_warn "Cachegrind failed for $bench (continuing)"
        fi
    else
        if ! "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR"; then
            echo "RUN_FAIL" >> "$status_file"
            pretty_warn "Hyperfine failed for $bench (continuing)"
        fi
    fi

    echo ""
}

reap_finished_job() {
    local i
    for i in "${!JOB_PIDS[@]}"; do
        local pid="${JOB_PIDS[$i]}"
        local state
        state=$(ps -p "$pid" -o stat= 2>/dev/null | tr -d '[:space:]')
        if [ -z "$state" ] || [[ "$state" == Z* ]]; then
            wait "$pid" || true
            unset 'JOB_PIDS[$i]'
            JOB_PIDS=("${JOB_PIDS[@]}")
            return 0
        fi
    done
    return 1
}

wait_for_available_slot() {
    while [ "${#JOB_PIDS[@]}" -ge "$JOB_COUNT" ]; do
        if ! reap_finished_job; then
            sleep 0.1
        fi
    done
}

wait_for_all_jobs() {
    local pid
    for pid in "${JOB_PIDS[@]}"; do
        wait "$pid" || true
    done
    JOB_PIDS=()
}

for bench in $BENCHMARKS; do
    if [ "$JOB_COUNT" -le 1 ]; then
        run_benchmark_job "$bench"
    else
        wait_for_available_slot
        run_benchmark_job "$bench" &
        JOB_PIDS+=("$!")
    fi
done

if [ "$JOB_COUNT" -gt 1 ]; then
    wait_for_all_jobs
fi

for bench in $BENCHMARKS; do
    status_file="$STATUS_DIR/${bench}.status"
    if [ ! -f "$status_file" ]; then
        RUN_FAILURES+=("$bench")
        continue
    fi
    if grep -q "BUILD_FAIL" "$status_file"; then
        BUILD_FAILURES+=("$bench")
    fi
    if grep -q "RUN_FAIL" "$status_file"; then
        RUN_FAILURES+=("$bench")
    fi
done
rm -rf "$STATUS_DIR"

# Process results
pretty_info "Processing results..."
    if [ "$USE_CACHEGRIND" = true ]; then
        if [ "$REFRESH_BASELINE" = "false" ]; then
            if ! python3 "$SCRIPT_DIR/infrastructure/cachegrind_processor.py" "$OUTPUT_DIR" --use-baseline; then
                PROCESS_FAILURES+=("cachegrind_processor")
                pretty_warn "cachegrind_processor failed (continuing)"
            fi
        else
            if ! python3 "$SCRIPT_DIR/infrastructure/cachegrind_processor.py" "$OUTPUT_DIR"; then
                PROCESS_FAILURES+=("cachegrind_processor")
                pretty_warn "cachegrind_processor failed (continuing)"
            fi
        fi
        # Update history log with cachegrind results
        if ! python3 "$SCRIPT_DIR/infrastructure/history_updater.py" "$OUTPUT_DIR"; then
            PROCESS_FAILURES+=("history_updater")
            pretty_warn "history_updater failed (continuing)"
        fi
    else
        if ! python3 "$SCRIPT_DIR/infrastructure/result_processor.py" "$OUTPUT_DIR"; then
            PROCESS_FAILURES+=("result_processor")
            pretty_warn "result_processor failed (continuing)"
        fi
    fi

echo ""
pretty_ok "Results saved to: $OUTPUT_DIR"
if [ "$USE_CACHEGRIND" = true ]; then
    pretty_info "Summary: $OUTPUT_DIR/cachegrind_summary.md"
else
    pretty_info "Summary: $OUTPUT_DIR/summary.md"
fi

if [ ${#BUILD_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
fi


if [ ${#RUN_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Benchmark run failures: ${RUN_FAILURES[*]}"
fi

if [ ${#PROCESS_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Processing failures: ${PROCESS_FAILURES[*]}"
fi

if [ ${#BUILD_FAILURES[@]} -ne 0 ] || [ ${#RUN_FAILURES[@]} -ne 0 ] || [ ${#PROCESS_FAILURES[@]} -ne 0 ]; then
    exit 1
fi
