#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [--cachegrind] [benchmark_name|all]
#
# Options:
#   --cachegrind  Use Valgrind Cachegrind for deterministic instruction counts
#                 (default: use hyperfine for timing)

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Parse options
USE_CACHEGRIND=false
BENCHMARK="all"

while [[ $# -gt 0 ]]; do
    case $1 in
        --cachegrind)
            USE_CACHEGRIND=true
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
echo "Recording compiler version..."
git -C "$PROJECT_ROOT" rev-parse HEAD > "$OUTPUT_DIR/compiler_version.txt"
git -C "$PROJECT_ROOT" log -1 --format="%s" >> "$OUTPUT_DIR/compiler_version.txt"

# Get list of benchmarks to run
if [ "$BENCHMARK" = "all" ]; then
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/ 2>/dev/null | xargs -n1 basename)
else
    BENCHMARKS="$BENCHMARK"
fi

if [ "$USE_CACHEGRIND" = true ]; then
    echo "Mode: Cachegrind (instruction counts)"
else
    echo "Mode: Hyperfine (timing)"
fi
echo "Benchmarks to run: $BENCHMARKS"
echo ""

for bench in $BENCHMARKS; do
    echo "=========================================="
    echo "Benchmark: $bench"
    echo "=========================================="

    # Build all implementations
    "$SCRIPT_DIR/infrastructure/build_all.sh" "$bench"

    # Validate correctness
    "$SCRIPT_DIR/infrastructure/validate_all.sh" "$bench"

    # Run benchmark
    if [ "$USE_CACHEGRIND" = true ]; then
        "$SCRIPT_DIR/infrastructure/cachegrind_runner.sh" "$bench" "$OUTPUT_DIR"
    else
        "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR"
    fi

    echo ""
done

# Process results
echo "Processing results..."
if [ "$USE_CACHEGRIND" = true ]; then
    python3 "$SCRIPT_DIR/infrastructure/cachegrind_processor.py" "$OUTPUT_DIR"
    # Update history log with cachegrind results
    python3 "$SCRIPT_DIR/infrastructure/history_updater.py" "$OUTPUT_DIR"
else
    python3 "$SCRIPT_DIR/infrastructure/result_processor.py" "$OUTPUT_DIR"
fi

echo ""
echo "Results saved to: $OUTPUT_DIR"
if [ "$USE_CACHEGRIND" = true ]; then
    echo "Summary: $OUTPUT_DIR/cachegrind_summary.md"
else
    echo "Summary: $OUTPUT_DIR/summary.md"
fi
