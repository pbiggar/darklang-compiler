#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [--hyperfine] [--verify|--verify-parent|--verify-fresh] [--quiet|--verbose] [--skip-smoke] [--reset-dark-baseline] [--refresh-baseline=rust] [--jobs[=N]] [full|benchmark_name|all]
#
# Options:
#   --help                   Show this help message and exit
#   --hyperfine              Use hyperfine for timing (default: cachegrind for instruction counts)
#   --verify                 Read-only verification against the canonical snapshot
#   --verify-parent          Read-only full verification against the branch parent
#   --verify-fresh           Read-only integration gate; an unrecorded improvement fails
#   --quiet                  Print only phase summaries, failures, and result locations
#   --verbose                Print per-benchmark details (verification is quiet by default)
#   --skip-smoke             Skip the cache-free smoke gate only when the caller has
#                            already passed it on the exact unchanged commit
#   --reset-dark-baseline    Replace Dark full snapshot from one complete successful run
#   --refresh-baseline=rust  Independently refresh audited Rust reference rows
#   --jobs, --jobs=N         Build and measure up to N benchmarks in parallel (default: 1)
#   --list                   Print the benchmarks that would run and exit

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"
source "$SCRIPT_DIR/infrastructure/parallel_jobs.sh"

machine_arch() {
    case "$(uname -m)" in
        x86_64|amd64) echo "x86_64" ;;
        aarch64|arm64) echo "arm64" ;;
        *) uname -m ;;
    esac
}

FULL_TRACK="$(machine_arch)-full-cachegrind"

show_help() {
    sed -n '/^# Usage:/,/^$/ {
        s/^# \{0,1\}//
        p
    }' "$0"
}

# Parse options
USE_CACHEGRIND=true
REFRESH_BASELINE=false
BENCHMARK="full"
BUILD_FAILURES=()
RUN_FAILURES=()
PROCESS_FAILURES=()
LIST_ONLY=false
VERIFY_RESULTS=false
VERIFY_PARENT=false
VERIFY_FRESH=false
RESET_DARK_BASELINE=false
SNAPSHOT_OVERRIDE=""
JOB_COUNT=""
SKIP_BENCHMARKS=()
PROFILE=""
SKIP_SMOKE=false
OUTPUT_MODE="default"

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
        --verify-parent)
            VERIFY_RESULTS=true
            VERIFY_PARENT=true
            shift
            ;;
        --verify-fresh)
            VERIFY_RESULTS=true
            VERIFY_FRESH=true
            shift
            ;;
        --quiet)
            OUTPUT_MODE="quiet"
            shift
            ;;
        --verbose)
            OUTPUT_MODE="verbose"
            shift
            ;;
        --skip-smoke)
            SKIP_SMOKE=true
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

QUIET_MODE=false
if [ "$OUTPUT_MODE" = "quiet" ] || { [ "$OUTPUT_MODE" = "default" ] && [ "$VERIFY_RESULTS" = true ]; }; then
    QUIET_MODE=true
fi

run_quiet_on_success() {
    local output
    if output=$("$@" 2>&1); then
        return 0
    fi
    printf '%s\n' "$output" >&2
    return 1
}

if [ "$VERIFY_RESULTS" = true ] && [ "$USE_CACHEGRIND" != true ]; then
    pretty_fail "--verify cannot be combined with --hyperfine"
    exit 1
fi

if [ "$VERIFY_PARENT" = true ] && [ "$VERIFY_FRESH" = true ]; then
    pretty_fail "--verify-parent cannot be combined with --verify-fresh"
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

if [ "$VERIFY_RESULTS" = true ] && [ "$BENCHMARK" != "full" ]; then
    pretty_fail "--verify requires the full benchmark profile"
    exit 1
fi

if [ "$RESET_DARK_BASELINE" = true ] && [ "$BENCHMARK" != "full" ]; then
    pretty_fail "--reset-dark-baseline requires the full profile"
    exit 1
fi

if [ "$USE_CACHEGRIND" = true ] && [ "$REFRESH_BASELINE" != "false" ] && [ "$BENCHMARK" != "full" ]; then
    pretty_fail "cachegrind baseline refresh requires the full benchmark profile"
    exit 1
fi

# Get list of benchmarks to run
if [ "$BENCHMARK" = "full" ]; then
    PROFILE="full"
    if ! BENCHMARKS=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_profiles.py" "$PROFILE"); then
        exit 1
    fi
elif [ "$BENCHMARK" = "all" ]; then
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/ 2>/dev/null | xargs -n1 basename)
else
    BENCHMARKS="$BENCHMARK"
fi

if [ -n "$PROFILE" ]; then
    if [ "$QUIET_MODE" = true ]; then
        run_quiet_on_success python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check-profile "$PROFILE" || exit 1
    elif ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check-profile "$PROFILE"; then
        exit 1
    fi
else
    if [ "$QUIET_MODE" = true ]; then
        run_quiet_on_success python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check || exit 1
    elif ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check; then
        exit 1
    fi
fi

if [ "$PROFILE" = "full" ] && [ "$USE_CACHEGRIND" = true ] && [ "$RESET_DARK_BASELINE" = false ] && [ "$LIST_ONLY" = false ]; then
    BASELINE_RELATIVE="benchmarks/baselines/dark-${FULL_TRACK}.json"
    if [ "$VERIFY_RESULTS" = false ] && git -C "$PROJECT_ROOT" ls-files -u -- "$BASELINE_RELATIVE" | grep -q .; then
        SNAPSHOT_OVERRIDE="$(mktemp "${TMPDIR:-/tmp}/dark-benchmark-baseline.XXXXXX.json")"
        trap 'rm -f "$SNAPSHOT_OVERRIDE"' EXIT
        git -C "$PROJECT_ROOT" show ":2:$BASELINE_RELATIVE" > "$SNAPSHOT_OVERRIDE"
    fi
    BASELINE_VALIDATE_ARGS=()
    if [ -n "$SNAPSHOT_OVERRIDE" ]; then
        BASELINE_VALIDATE_ARGS+=(--snapshot-override "$SNAPSHOT_OVERRIDE")
    fi
    if [ "$QUIET_MODE" = true ]; then
        run_quiet_on_success python3 "$SCRIPT_DIR/infrastructure/benchmark_baseline.py" validate \
            --benchmarks-dir "$SCRIPT_DIR" --language dark --track "$FULL_TRACK" "${BASELINE_VALIDATE_ARGS[@]}" || exit 1
    elif ! python3 "$SCRIPT_DIR/infrastructure/benchmark_baseline.py" validate \
        --benchmarks-dir "$SCRIPT_DIR" --language dark --track "$FULL_TRACK" "${BASELINE_VALIDATE_ARGS[@]}"; then
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
LOG_DIR="$OUTPUT_DIR/logs"
mkdir -p "$LOG_DIR/build" "$LOG_DIR/measurement"

# Record compiler version
if [ "$QUIET_MODE" != true ]; then
    pretty_info "Recording compiler version..."
fi
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

if [ "$QUIET_MODE" != true ]; then
    pretty_info "Building current Dark compiler..."
fi
if [ "$QUIET_MODE" = true ]; then
    if ! dotnet build "$PROJECT_ROOT/src/DarkCompiler/DarkCompiler.fsproj" --no-incremental --verbosity quiet >"$LOG_DIR/compiler-build.log" 2>&1; then
        cat "$LOG_DIR/compiler-build.log"
        pretty_fail "Dark compiler build failed"
        exit 1
    fi
elif ! dotnet build "$PROJECT_ROOT/src/DarkCompiler/DarkCompiler.fsproj" --no-incremental --verbosity quiet; then
    pretty_fail "Dark compiler build failed"
    exit 1
fi

STATUS_DIR="$OUTPUT_DIR/status"
mkdir -p "$STATUS_DIR"

if [ "$QUIET_MODE" = true ]; then
    BENCHMARK_COUNT=${#FILTERED_BENCHMARKS[@]}
    if [ "$VERIFY_RESULTS" = true ]; then
        pretty_section "Benchmark verification: $BENCHMARK_COUNT workloads, $JOB_COUNT job(s)"
    else
        pretty_section "Benchmark run: $BENCHMARK_COUNT workloads, $JOB_COUNT job(s)"
    fi
elif [ "$USE_CACHEGRIND" = true ]; then
    if [ "$REFRESH_BASELINE" = "false" ]; then
        pretty_section "Mode: Cachegrind (instruction counts) - Dark only (use --refresh-baseline for baselines)"
    else
        pretty_section "Mode: Cachegrind (instruction counts) - refreshing: $REFRESH_BASELINE"
    fi
else
    pretty_section "Mode: Hyperfine (timing)"
fi
if [ "$QUIET_MODE" != true ]; then
    pretty_info "Benchmarks to run: $BENCHMARKS"
    pretty_info "Parallel jobs: $JOB_COUNT"
    if [ "${#SKIPPED_BENCHMARKS[@]}" -ne 0 ]; then
        pretty_warn "Skipping benchmarks: ${SKIPPED_BENCHMARKS[*]}"
    fi
    echo ""
fi

build_baseline_job() {
    local bench="$1"
    local status_file="$STATUS_DIR/${bench}.status"
    local build_log="$LOG_DIR/build/${bench}.log"
    local build_args=(--skip-dark)
    if [ "$QUIET_MODE" = true ]; then
        if ! "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench" "${build_args[@]}" >"$build_log" 2>&1; then
            echo "BUILD_FAIL" >> "$status_file"
            pretty_warn "Build failed for $bench (log: $build_log)"
        fi
    elif ! "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench" "${build_args[@]}"; then
        echo "BUILD_FAIL" >> "$status_file"
        pretty_warn "Build failed for $bench"
    fi
}

run_benchmark_job() {
    local bench="$1"
    local status_file="$STATUS_DIR/${bench}.status"
    local parity_status
    local dark_binary="$OUTPUT_DIR/binaries/$bench/dark/main"
    local measurement_log="$LOG_DIR/measurement/${bench}.log"
    if ! parity_status=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" status "$bench"); then
        echo "RUN_FAIL" >> "$status_file"
        pretty_warn "Parity status unavailable for $bench"
        return
    fi

    if [ "$QUIET_MODE" != true ]; then
        pretty_header "Benchmark: $bench"
    fi

    if [ "$USE_CACHEGRIND" = true ]; then
        if [ "$QUIET_MODE" = true ]; then
            if ! "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status" "$REFRESH_BASELINE" "$dark_binary" full >"$measurement_log" 2>&1; then
                echo "RUN_FAIL" >> "$status_file"
                pretty_warn "Cachegrind failed for $bench (log: $measurement_log)"
            fi
        elif ! "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status" "$REFRESH_BASELINE" "$dark_binary" full; then
            echo "RUN_FAIL" >> "$status_file"
            pretty_warn "Cachegrind failed for $bench (continuing)"
        fi
    else
        if [ "$QUIET_MODE" = true ]; then
            if ! "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status" "$dark_binary" full >"$measurement_log" 2>&1; then
                echo "RUN_FAIL" >> "$status_file"
                pretty_warn "Hyperfine failed for $bench (log: $measurement_log)"
            fi
        elif ! "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR" "$parity_status" "$dark_binary" full; then
            echo "RUN_FAIL" >> "$status_file"
            pretty_warn "Hyperfine failed for $bench (continuing)"
        fi
    fi

    if [ "$QUIET_MODE" != true ]; then
        echo ""
    fi
}

if [ "$QUIET_MODE" != true ]; then
    pretty_section "Build gate"
fi

for bench in "${FILTERED_BENCHMARKS[@]}"; do
    : > "$STATUS_DIR/${bench}.status"
done

DARK_BATCH_LOG="$LOG_DIR/build/dark-batch.log"
if [ "$QUIET_MODE" = true ]; then
    if ! "$SCRIPT_DIR/infrastructure/build_dark_batch.sh" \
        --output-dir="$OUTPUT_DIR/binaries" "${FILTERED_BENCHMARKS[@]}" >"$DARK_BATCH_LOG" 2>&1; then
        pretty_warn "Dark batch build failed (log: $DARK_BATCH_LOG)"
        for bench in "${FILTERED_BENCHMARKS[@]}"; do
            echo "BUILD_FAIL" >> "$STATUS_DIR/${bench}.status"
        done
    fi
else
    pretty_info "Building Dark benchmarks with one shared stdlib..."
    if "$SCRIPT_DIR/infrastructure/build_dark_batch.sh" \
        --output-dir="$OUTPUT_DIR/binaries" "${FILTERED_BENCHMARKS[@]}"; then
        pretty_ok "Dark batch build complete"
    else
        pretty_warn "Dark batch build failed"
        for bench in "${FILTERED_BENCHMARKS[@]}"; do
            echo "BUILD_FAIL" >> "$STATUS_DIR/${bench}.status"
        done
    fi
fi

BUILD_BASELINES=false
if [ "$USE_CACHEGRIND" != true ] || [ "$REFRESH_BASELINE" != "false" ]; then
    BUILD_BASELINES=true
fi

if [ "$BUILD_BASELINES" = true ] && ! grep -q "BUILD_FAIL" "$STATUS_DIR"/*.status; then
    run_parallel_jobs "$JOB_COUNT" build_baseline_job "${FILTERED_BENCHMARKS[@]}"
fi

for bench in $BENCHMARKS; do
    status_file="$STATUS_DIR/${bench}.status"
    if [ ! -f "$status_file" ] || grep -q "BUILD_FAIL" "$status_file"; then
        BUILD_FAILURES+=("$bench")
    fi
done

if [ ${#BUILD_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
    pretty_fail "Build gate failed; Cachegrind was not started and canonical reports were not changed"
    exit 1
fi

if [ "$SKIP_SMOKE" != true ]; then
    SMOKE_BENCHMARKS=$(IFS=,; echo "${FILTERED_BENCHMARKS[*]}")
    if ! "$SCRIPT_DIR/quick_check.sh" --smoke --quiet \
        --benchmarks="$SMOKE_BENCHMARKS" --prebuilt-dir="$OUTPUT_DIR/binaries"; then
        pretty_fail "Canonical smoke gate failed; Cachegrind was not started"
        exit 1
    fi
fi

if [ "$QUIET_MODE" != true ]; then
    pretty_section "Measurement gate"
fi
run_parallel_jobs "$JOB_COUNT" run_benchmark_job "${FILTERED_BENCHMARKS[@]}"

for bench in $BENCHMARKS; do
    status_file="$STATUS_DIR/${bench}.status"
    if [ ! -f "$status_file" ]; then
        RUN_FAILURES+=("$bench")
        continue
    fi
    if grep -q "RUN_FAIL" "$status_file"; then
        RUN_FAILURES+=("$bench")
    fi
done
rm -rf "$STATUS_DIR"

# No result processor or tracked-file recorder may observe an incomplete build/run.
if [ ${#RUN_FAILURES[@]} -ne 0 ]; then
    if [ ${#RUN_FAILURES[@]} -ne 0 ]; then
        pretty_fail "Benchmark run failures: ${RUN_FAILURES[*]}"
    fi
    pretty_fail "Incomplete run; canonical snapshots and tracked reports were not changed"
    exit 1
fi

# Process results
if [ "$QUIET_MODE" != true ]; then
    pretty_info "Processing results..."
fi
    if [ "$USE_CACHEGRIND" = true ]; then
        PROCESSOR_ARGS=()
        if [ "$QUIET_MODE" = true ]; then
            PROCESSOR_ARGS+=(--quiet)
        fi
        if [ "$REFRESH_BASELINE" = "false" ]; then
            if ! python3 "$SCRIPT_DIR/infrastructure/cachegrind_processor.py" "$OUTPUT_DIR" --use-baseline "${PROCESSOR_ARGS[@]}"; then
                PROCESS_FAILURES+=("cachegrind_processor")
                pretty_warn "cachegrind_processor failed (continuing)"
            fi
        else
            if ! python3 "$SCRIPT_DIR/infrastructure/cachegrind_processor.py" "$OUTPUT_DIR" "${PROCESSOR_ARGS[@]}"; then
                PROCESS_FAILURES+=("cachegrind_processor")
                pretty_warn "cachegrind_processor failed (continuing)"
            fi
        fi
        if [ "$VERIFY_RESULTS" = true ]; then
            if [ "$VERIFY_PARENT" = true ]; then
                PARENT_ARGS=()
                if [ "$QUIET_MODE" = true ]; then
                    PARENT_ARGS+=(--quiet)
                fi
                if ! python3 "$SCRIPT_DIR/compare_with_parent.py" "$OUTPUT_DIR" "${PARENT_ARGS[@]}"; then
                    PROCESS_FAILURES+=("parent_comparison")
                    pretty_warn "task-parent benchmark verification failed"
                fi
            else
                VERIFY_ARGS=()
                if [ "$VERIFY_FRESH" = true ]; then
                    VERIFY_ARGS+=(--require-recorded)
                fi
                if [ "$QUIET_MODE" = true ]; then
                    VERIFY_ARGS+=(--quiet)
                fi
                if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_verifier.py" "$OUTPUT_DIR" "$PROFILE" "${VERIFY_ARGS[@]}"; then
                    PROCESS_FAILURES+=("benchmark_verifier")
                    pretty_warn "benchmark verification failed"
                fi
            fi
        elif [ "$PROFILE" = "full" ]; then
            # Only a successful full run updates canonical current-state files.
            HISTORY_REFRESH_ARGS=()
            if [ "$REFRESH_BASELINE" != "false" ]; then
                HISTORY_REFRESH_ARGS+=(--refresh-baseline)
            fi
            HISTORY_RESET_ARGS=()
            if [ "$RESET_DARK_BASELINE" = true ]; then
                HISTORY_RESET_ARGS+=(--reset-dark-baseline)
            fi
            if [ -n "$SNAPSHOT_OVERRIDE" ]; then
                HISTORY_RESET_ARGS+=(--snapshot-override "$SNAPSHOT_OVERRIDE")
            fi
            if ! python3 "$SCRIPT_DIR/infrastructure/history_updater.py" "$OUTPUT_DIR" --profile "$PROFILE" "${HISTORY_REFRESH_ARGS[@]}" "${HISTORY_RESET_ARGS[@]}"; then
                PROCESS_FAILURES+=("history_updater")
                pretty_warn "history_updater failed (continuing)"
            fi
        else
            pretty_info "Diagnostic target complete; canonical current-state files were not updated."
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
