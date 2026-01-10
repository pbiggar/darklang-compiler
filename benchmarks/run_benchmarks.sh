#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [--hyperfine] [--refresh-baseline[=lang1,lang2]] [benchmark_name|all]
#
# Options:
#   --hyperfine              Use hyperfine for timing (default: cachegrind for instruction counts)
#   --refresh-baseline       Re-run all baseline languages (default: use cached values)
#   --refresh-baseline=LANGS Re-run specific languages only (comma-separated: rust,go,python,node,ocaml)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"

# Parse options
USE_CACHEGRIND=true
export REFRESH_BASELINE=false
BENCHMARK="all"
BUILD_FAILURES=()
VALIDATION_FAILURES=()
RUN_FAILURES=()
PROCESS_FAILURES=()
VALIDATION_FAILURES=()

while [[ $# -gt 0 ]]; do
    case $1 in
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
echo ""

for bench in $BENCHMARKS; do
    pretty_header "Benchmark: $bench"

    # Build all implementations
    if ! "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench"; then
        BUILD_FAILURES+=("$bench")
        pretty_warn "Build failed for $bench (continuing)"
    fi

    # Validate correctness
    if ! "$SCRIPT_DIR/infrastructure/validate_all.sh" "$bench"; then
        VALIDATION_FAILURES+=("$bench")
        pretty_warn "Validation failed for $bench (continuing)"
    fi

    # Run benchmark
    if [ "$USE_CACHEGRIND" = true ]; then
        if ! "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR"; then
            RUN_FAILURES+=("$bench")
            pretty_warn "Cachegrind failed for $bench (continuing)"
        fi
    else
        if ! "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR"; then
            RUN_FAILURES+=("$bench")
            pretty_warn "Hyperfine failed for $bench (continuing)"
        fi
    fi

    echo ""
done

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

if [ ${#VALIDATION_FAILURES[@]} -ne 0 ]; then
    echo ""
    pretty_fail "Validation failed for: ${VALIDATION_FAILURES[*]}"
fi

if [ ${#RUN_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Benchmark run failures: ${RUN_FAILURES[*]}"
fi

if [ ${#PROCESS_FAILURES[@]} -ne 0 ]; then
    pretty_fail "Processing failures: ${PROCESS_FAILURES[*]}"
fi

if [ ${#BUILD_FAILURES[@]} -ne 0 ] || [ ${#VALIDATION_FAILURES[@]} -ne 0 ] || [ ${#RUN_FAILURES[@]} -ne 0 ] || [ ${#PROCESS_FAILURES[@]} -ne 0 ]; then
    exit 1
fi
