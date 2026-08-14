#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [--hyperfine] [--verify|--verify-fresh] [--reset-dark-baseline] [--refresh-baseline=rust] [--machine=ID] [--jobs[=N]] [routine|benchmark_name|all]
#
# Options:
#   --help                   Show this help message and exit
#   --hyperfine              Use hyperfine for timing (default: cachegrind for instruction counts)
#   --verify                 Read-only routine verification; equal or improved suites pass
#   --verify-fresh           Read-only integration gate; an unrecorded improvement fails
#   --reset-dark-baseline    Replace Dark routine snapshot from one complete successful run
#   --refresh-baseline=rust  Independently refresh audited Rust reference rows
#   --machine=ID             Optional machine registry ID for recorded history
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
REFRESH_BASELINE=false
BENCHMARK="routine"
BUILD_FAILURES=()
RUN_FAILURES=()
PROCESS_FAILURES=()
LIST_ONLY=false
VERIFY_RESULTS=false
VERIFY_FRESH=false
RESET_DARK_BASELINE=false
JOB_COUNT=""
SKIP_BENCHMARKS=()
PROFILE=""
MACHINE_ID=""

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
        --verify)
            VERIFY_RESULTS=true
            shift
            ;;
        --verify-fresh)
            VERIFY_RESULTS=true
            VERIFY_FRESH=true
            shift
            ;;
        --reset-dark-baseline)
            RESET_DARK_BASELINE=true
            shift
            ;;
        --refresh-baseline)
            REFRESH_BASELINE="rust"
            shift
            ;;
        --refresh-baseline=*)
            REFRESH_BASELINE="${1#*=}"
            shift
            ;;
        --machine)
            if [ -z "${2:-}" ]; then
                pretty_fail "--machine requires a value"
                exit 1
            fi
            MACHINE_ID="$2"
            shift 2
            ;;
        --machine=*)
            MACHINE_ID="${1#*=}"
            if [ -z "$MACHINE_ID" ]; then
                pretty_fail "--machine requires a value"
                exit 1
            fi
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

if [ "$VERIFY_RESULTS" = true ] && [ "$USE_CACHEGRIND" != true ]; then
    pretty_fail "--verify cannot be combined with --hyperfine"
    exit 1
fi

if [ "$RESET_DARK_BASELINE" = true ] && [ "$VERIFY_RESULTS" = true ]; then
    pretty_fail "--reset-dark-baseline cannot be combined with verification"
    exit 1
fi

if [ "$RESET_DARK_BASELINE" = true ] && [ "$USE_CACHEGRIND" != true ]; then
    pretty_fail "--reset-dark-baseline requires Cachegrind"
    exit 1
fi

if [ "$VERIFY_RESULTS" = true ] && [ "$REFRESH_BASELINE" != "false" ]; then
    pretty_fail "--verify cannot be combined with --refresh-baseline"
    exit 1
fi

if [ "$REFRESH_BASELINE" != "false" ] && [ "$REFRESH_BASELINE" != "rust" ]; then
    pretty_fail "Only audited Rust baselines can be refreshed"
    exit 1
fi

if [ "$VERIFY_RESULTS" = true ] && [ "$BENCHMARK" != "routine" ]; then
    pretty_fail "--verify requires the routine benchmark profile"
    exit 1
fi

if [ "$RESET_DARK_BASELINE" = true ] && [ "$BENCHMARK" != "routine" ]; then
    pretty_fail "--reset-dark-baseline requires the complete routine profile"
    exit 1
fi

if [ "$USE_CACHEGRIND" = true ] && [ "$REFRESH_BASELINE" != "false" ] && [ "$BENCHMARK" != "routine" ]; then
    pretty_fail "cachegrind baseline refresh requires the routine benchmark profile"
    exit 1
fi

# Get list of benchmarks to run
if [ "$BENCHMARK" = "routine" ]; then
    PROFILE="routine"
    if ! BENCHMARKS=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_profiles.py" "$PROFILE"); then
        exit 1
    fi
elif [ "$BENCHMARK" = "all" ]; then
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/ 2>/dev/null | xargs -n1 basename)
else
    BENCHMARKS="$BENCHMARK"
fi

if [ -n "$PROFILE" ]; then
    if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check-profile "$PROFILE"; then
        exit 1
    fi
else
    if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check; then
        exit 1
    fi
fi

if [ "$PROFILE" = "routine" ] && [ "$USE_CACHEGRIND" = true ] && [ "$RESET_DARK_BASELINE" = false ] && [ "$LIST_ONLY" = false ]; then
    if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_baseline.py" validate \
        --benchmarks-dir "$SCRIPT_DIR" --architecture "$(uname -m)" --profile routine; then
        exit 1
    fi
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

OUTPUT_DIR="$SCRIPT_DIR/results/$(date +%Y-%m-%d_%H%M%S)"
mkdir -p "$OUTPUT_DIR"

# Record compiler version
pretty_info "Recording compiler version..."
git -C "$PROJECT_ROOT" rev-parse HEAD > "$OUTPUT_DIR/compiler_version.txt"
git -C "$PROJECT_ROOT" log -1 --format="%s" >> "$OUTPUT_DIR/compiler_version.txt"
date -u -Iseconds > "$OUTPUT_DIR/run_timestamp.txt"
printf '%s-%s-%s\n' "$(date -u +%Y%m%dT%H%M%SZ)" "$$" "$(git -C "$PROJECT_ROOT" rev-parse --short=12 HEAD)" > "$OUTPUT_DIR/run_identity.txt"

if [ -z "$JOB_COUNT" ]; then
    JOB_COUNT=1
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

pretty_info "Building current Dark compiler..."
if ! dotnet build "$PROJECT_ROOT/src/DarkCompiler/DarkCompiler.fsproj" --verbosity quiet; then
    pretty_fail "Dark compiler build failed"
    exit 1
fi

STATUS_DIR="$OUTPUT_DIR/status"
mkdir -p "$STATUS_DIR"

if [ "$USE_CACHEGRIND" = true ]; then
    if [ "$REFRESH_BASELINE" = "false" ]; then
        pretty_section "Mode: Cachegrind (instruction counts) - Dark only (use --refresh-baseline for baselines)"
    else
        pretty_section "Mode: Cachegrind (instruction counts) - refreshing: $REFRESH_BASELINE"
    fi
else
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
    local parity_status
    if ! parity_status=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" status "$bench"); then
        echo "BUILD_FAIL" >> "$status_file"
        pretty_warn "Parity status unavailable for $bench"
        return
    fi
    : > "$status_file"

    pretty_header "Benchmark: $bench"

    # Build all implementations
    local build_args=()
    if [ "$USE_CACHEGRIND" = true ] && [ "$REFRESH_BASELINE" = "false" ]; then
        build_args+=(--skip-baselines)
    fi
    if ! "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench" "${build_args[@]}"; then
        echo "BUILD_FAIL" >> "$status_file"
        pretty_warn "Build failed for $bench (continuing)"
    fi

    # Run benchmark
    if [ "$USE_CACHEGRIND" = true ]; then
        if ! "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status" "$REFRESH_BASELINE"; then
            echo "RUN_FAIL" >> "$status_file"
            pretty_warn "Cachegrind failed for $bench (continuing)"
        fi
    else
        if ! "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status"; then
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

# No result processor or tracked-file recorder may observe an incomplete build/run.
if [ ${#BUILD_FAILURES[@]} -ne 0 ] || [ ${#RUN_FAILURES[@]} -ne 0 ]; then
    if [ ${#BUILD_FAILURES[@]} -ne 0 ]; then
        pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
    fi
    if [ ${#RUN_FAILURES[@]} -ne 0 ]; then
        pretty_fail "Benchmark run failures: ${RUN_FAILURES[*]}"
    fi
    pretty_fail "Incomplete run; canonical snapshots and tracked reports were not changed"
    exit 1
fi

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
        if [ "$VERIFY_RESULTS" = true ]; then
            VERIFY_ARGS=()
            if [ "$VERIFY_FRESH" = true ]; then
                VERIFY_ARGS+=(--require-recorded)
            fi
            if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_verifier.py" "$OUTPUT_DIR" "$PROFILE" "${VERIFY_ARGS[@]}"; then
                PROCESS_FAILURES+=("benchmark_verifier")
                pretty_warn "benchmark verification failed"
            fi
        elif [ "$PROFILE" = "routine" ]; then
            # Only a complete routine run updates the canonical results and history.
            HISTORY_REFRESH_ARGS=()
            if [ "$REFRESH_BASELINE" != "false" ]; then
                HISTORY_REFRESH_ARGS+=(--refresh-baseline)
            fi
            HISTORY_MACHINE_ARGS=()
            if [ -n "$MACHINE_ID" ]; then
                HISTORY_MACHINE_ARGS+=(--machine "$MACHINE_ID")
            fi
            HISTORY_RESET_ARGS=()
            if [ "$RESET_DARK_BASELINE" = true ]; then
                HISTORY_RESET_ARGS+=(--reset-dark-baseline)
            fi
            if ! python3 "$SCRIPT_DIR/infrastructure/history_updater.py" "$OUTPUT_DIR" --profile "$PROFILE" "${HISTORY_MACHINE_ARGS[@]}" "${HISTORY_REFRESH_ARGS[@]}" "${HISTORY_RESET_ARGS[@]}"; then
                PROCESS_FAILURES+=("history_updater")
                pretty_warn "history_updater failed (continuing)"
            fi
        else
            pretty_info "Diagnostic target complete; RESULTS.md and HISTORY.md were not updated."
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
