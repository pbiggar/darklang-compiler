#!/bin/bash
# Main entry point for running benchmarks
# Usage: ./benchmarks/run_benchmarks.sh [benchmark_name|all]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BENCHMARK=${1:-all}
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
    "$SCRIPT_DIR/infrastructure/hyperfine_runner.sh" "$bench" "$OUTPUT_DIR"

    echo ""
done

# Process results
echo "Processing results..."
python3 "$SCRIPT_DIR/infrastructure/result_processor.py" "$OUTPUT_DIR"

# Update history log
python3 "$SCRIPT_DIR/infrastructure/history_updater.py" "$OUTPUT_DIR"

echo ""
echo "Results saved to: $OUTPUT_DIR"
echo "Summary: $OUTPUT_DIR/summary.md"
