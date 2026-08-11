#!/bin/bash
# Quick benchmark check for regression detection
# Usage: ./benchmarks/quick_check.sh [--fast] [--save-baseline] [--build] [--quiet]
#
# Builds matching Dark and Rust reduced workloads, validates both outputs, and
# runs both under Cachegrind. Dark counts are checked for compiler regressions;
# Rust counts provide a same-conditions comparison for audited comparable pairs.
#
# Options:
#   --fast             Run only 5 key benchmarks (~5s instead of ~20s)
#   --save-baseline    Save current counts as new baseline (run after intentional changes)
#   --build            Force rebuild all benchmarks (otherwise only rebuilds if source changed)
#   --quiet            Quiet mode: print "success" or list regressions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"

machine_arch() {
    case "$(uname -m)" in
        x86_64|amd64)
            echo "x86_64"
            ;;
        aarch64|arm64)
            echo "arm64"
            ;;
        *)
            uname -m
            ;;
    esac
}

ARCH="$(machine_arch)"
ARCH_BASELINE_FILE="$SCRIPT_DIR/QUICK_BASELINE.$ARCH.txt"
LEGACY_BASELINE_FILE="$SCRIPT_DIR/QUICK_BASELINE.txt"
if [ -f "$ARCH_BASELINE_FILE" ]; then
    BASELINE_FILE="$ARCH_BASELINE_FILE"
elif [ "$ARCH" = "x86_64" ] && [ -f "$LEGACY_BASELINE_FILE" ]; then
    BASELINE_FILE="$LEGACY_BASELINE_FILE"
else
    BASELINE_FILE="$ARCH_BASELINE_FILE"
fi
SAVE_BASELINE=false
FORCE_BUILD=false
FAST_MODE=false
QUIET_MODE=false
REGRESSION_THRESHOLD=0  # Any increase is a regression (deterministic counts)

# Key benchmarks for fast mode (diverse coverage: recursion, loops, floats, lists, bitops)
FAST_BENCHMARKS="fib ackermann mandelbrot quicksort nqueen"

GENERATED_RUST_BINARIES=()

clean_generated_files() {
    rm -f "$PROJECT_ROOT"/cachegrind.out.*
    if [ "${#GENERATED_RUST_BINARIES[@]}" -gt 0 ]; then
        rm -f -- "${GENERATED_RUST_BINARIES[@]}"
    fi
}

trap clean_generated_files EXIT
rm -f "$PROJECT_ROOT"/cachegrind.out.*

# Parse options
while [[ $# -gt 0 ]]; do
    case $1 in
        --fast)
            FAST_MODE=true
            shift
            ;;
        --save-baseline)
            SAVE_BASELINE=true
            shift
            ;;
        --build)
            FORCE_BUILD=true
            shift
            ;;
        --quiet)
            QUIET_MODE=true
            shift
            ;;
        *)
            echo "Unknown option: $1"
            echo "Usage: $0 [--fast] [--save-baseline] [--build] [--quiet]"
            exit 1
            ;;
    esac
done

# Check for valgrind
if ! command -v valgrind &> /dev/null; then
    if [ "$QUIET_MODE" = true ]; then
        echo "valgrind not installed"
    else
        pretty_fail "valgrind is not installed"
        pretty_info "Install with: sudo apt-get install valgrind"
    fi
    exit 1
fi

if ! command -v rustc &> /dev/null; then
    pretty_fail "rustc is not installed"
    exit 1
fi

if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check; then
    pretty_fail "Benchmark parity contract failed"
    exit 1
fi

if ! dotnet build "$PROJECT_ROOT/src/DarkCompiler/DarkCompiler.fsproj" --verbosity quiet; then
    pretty_fail "Dark compiler build failed"
    exit 1
fi
COMPILER_DLL="$PROJECT_ROOT/bin/DarkCompiler/Debug/net10.0/DarkCompiler.dll"
if [ ! -f "$COMPILER_DLL" ]; then
    pretty_fail "Dark compiler output is missing: $COMPILER_DLL"
    exit 1
fi

if [ "$QUIET_MODE" != true ]; then
    if [ "$FAST_MODE" = true ]; then
        pretty_section "Quick benchmark check (fast mode - 5 benchmarks)"
    else
        pretty_section "Quick benchmark check for regression detection"
    fi
    pretty_info "Architecture: $ARCH"
    pretty_info "Baseline: $BASELINE_FILE"
    echo ""
fi

# Find all quick.dark files
if [ "$FAST_MODE" = true ]; then
    BENCHMARKS="$FAST_BENCHMARKS"
else
    BENCHMARKS=$(ls -d "$SCRIPT_DIR/problems"/*/dark/quick.dark 2>/dev/null | while read f; do
        basename "$(dirname "$(dirname "$f")")"
    done)
fi

if [ -z "$BENCHMARKS" ]; then
    pretty_fail "No quick.dark files found"
    exit 1
fi

# Load baseline if exists
declare -A BASELINE
HAS_BASELINE=false
if [ -f "$BASELINE_FILE" ] && [ "$SAVE_BASELINE" = false ]; then
    HAS_BASELINE=true
    while IFS='=' read -r name count; do
        if [[ "$name" = \#* ]]; then
            continue
        fi
        BASELINE["$name"]="$count"
    done < "$BASELINE_FILE"
fi

# Track results
declare -A RESULTS
declare -A RUST_RESULTS
FAILURES=()
BUILD_FAILURES=()
TOTAL_INSTRUCTIONS=0
TOTAL_RUST_INSTRUCTIONS=0
START_TIME=$(date +%s)

for bench in $BENCHMARKS; do
    PROBLEM_DIR="$SCRIPT_DIR/problems/$bench"
    QUICK_DARK="$PROBLEM_DIR/dark/quick.dark"
    QUICK_BIN="$PROBLEM_DIR/dark/quick"
    QUICK_RUST="$PROBLEM_DIR/rust/quick.rs"
    QUICK_RUST_BIN="$PROBLEM_DIR/rust/quick"
    GENERATED_RUST_BINARIES+=("$QUICK_RUST_BIN")
    QUICK_EXPECTED="$PROBLEM_DIR/quick_expected_output.txt"
    if ! QUICK_PARITY_STATUS=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" status "$bench" quick); then
        FAILURES+=("$bench: quick parity status unavailable")
        continue
    fi

    # Build only if needed (source newer than binary, or --build flag)
    NEEDS_BUILD=false
    if [ "$FORCE_BUILD" = true ]; then
        NEEDS_BUILD=true
    elif [ ! -x "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    elif [ "$QUICK_DARK" -nt "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    elif [ "$COMPILER_DLL" -nt "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    fi

    if [ "$NEEDS_BUILD" = true ]; then
        if ! "$PROJECT_ROOT/dark" "$QUICK_DARK" -o "$QUICK_BIN" -q 2>/dev/null; then
            BUILD_FAILURES+=("$bench")
            if [ "$QUIET_MODE" != true ]; then
                pretty_warn "$bench: build failed"
            fi
            continue
        fi
    fi

    RUST_NEEDS_BUILD=false
    if [ "$FORCE_BUILD" = true ]; then
        RUST_NEEDS_BUILD=true
    elif [ ! -x "$QUICK_RUST_BIN" ]; then
        RUST_NEEDS_BUILD=true
    elif [ "$QUICK_RUST" -nt "$QUICK_RUST_BIN" ]; then
        RUST_NEEDS_BUILD=true
    fi

    if [ "$RUST_NEEDS_BUILD" = true ]; then
        if ! rustc -C opt-level=3 "$QUICK_RUST" -o "$QUICK_RUST_BIN" 2>/dev/null; then
            BUILD_FAILURES+=("$bench/rust")
            if [ "$QUIET_MODE" != true ]; then
                pretty_warn "$bench: Rust quick build failed"
            fi
            continue
        fi
    fi

    if [ ! -f "$QUICK_EXPECTED" ]; then
        FAILURES+=("$bench: missing quick_expected_output.txt")
        continue
    fi

    EXPECTED_OUTPUT=$(cat "$QUICK_EXPECTED")
    if ! DARK_OUTPUT=$("$QUICK_BIN"); then
        FAILURES+=("$bench: Dark quick execution failed")
        continue
    fi
    if ! RUST_OUTPUT=$("$QUICK_RUST_BIN"); then
        FAILURES+=("$bench: Rust quick execution failed")
        continue
    fi
    if [ "$DARK_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
        FAILURES+=("$bench: Dark output mismatch (expected '$EXPECTED_OUTPUT', got '$DARK_OUTPUT')")
        continue
    fi
    if [ "$RUST_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
        FAILURES+=("$bench: Rust output mismatch (expected '$EXPECTED_OUTPUT', got '$RUST_OUTPUT')")
        continue
    fi

    # Run under cachegrind
    CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no "$QUICK_BIN" 2>&1)
    I_REFS=$(echo "$CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')

    if [ -z "$I_REFS" ]; then
        FAILURES+=("$bench: cachegrind failed")
        if [ "$QUIET_MODE" != true ]; then
            pretty_warn "$bench: cachegrind failed"
        fi
        continue
    fi

    RESULTS["$bench"]="$I_REFS"
    TOTAL_INSTRUCTIONS=$((TOTAL_INSTRUCTIONS + I_REFS))

    RUST_CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no "$QUICK_RUST_BIN" 2>&1)
    RUST_I_REFS=$(echo "$RUST_CG_OUTPUT" | grep "I refs:" | sed 's/.*I refs:[[:space:]]*//' | tr -d ',')
    if [ -z "$RUST_I_REFS" ]; then
        FAILURES+=("$bench: Rust cachegrind failed")
        if [ "$QUIET_MODE" != true ]; then
            pretty_warn "$bench: Rust cachegrind failed"
        fi
        continue
    fi
    RUST_RESULTS["$bench"]="$RUST_I_REFS"
    TOTAL_RUST_INSTRUCTIONS=$((TOTAL_RUST_INSTRUCTIONS + RUST_I_REFS))

    # Compare against baseline
    if [ -n "${BASELINE[$bench]}" ]; then
        BASELINE_COUNT="${BASELINE[$bench]}"
        DIFF=$((I_REFS - BASELINE_COUNT))
        if [ "$DIFF" -gt "$REGRESSION_THRESHOLD" ]; then
            FAILURES+=("$bench: regression +$DIFF instructions ($BASELINE_COUNT -> $I_REFS)")
            if [ "$QUIET_MODE" != true ]; then
                pretty_fail "$bench: $I_REFS (+$DIFF regression)"
            fi
        elif [ "$DIFF" -lt 0 ]; then
            if [ "$QUIET_MODE" != true ]; then
                pretty_ok "$bench: $I_REFS ($DIFF improvement)"
            fi
        else
            if [ "$QUIET_MODE" != true ]; then
                pretty_ok "$bench: $I_REFS (unchanged)"
            fi
        fi
    elif [ "$SAVE_BASELINE" = false ] && [ "$HAS_BASELINE" = true ]; then
        FAILURES+=("$bench: missing baseline in $BASELINE_FILE")
        if [ "$QUIET_MODE" != true ]; then
            pretty_fail "$bench: $I_REFS (missing baseline)"
        fi
    fi

    if [ "$QUIET_MODE" != true ] && [ "$QUICK_PARITY_STATUS" = "comparable" ]; then
        RATIO=$(awk -v dark="$I_REFS" -v rust="$RUST_I_REFS" 'BEGIN { printf "%.2f", dark / rust }')
        pretty_info "$bench Rust: $RUST_I_REFS instructions (Dark/Rust: ${RATIO}x)"
    elif [ "$QUIET_MODE" != true ]; then
        pretty_info "$bench Rust: $RUST_I_REFS instructions (diagnostic only: $QUICK_PARITY_STATUS)"
    fi
done

END_TIME=$(date +%s)
ELAPSED=$((END_TIME - START_TIME))

if [ "$QUIET_MODE" != true ]; then
    echo ""
    pretty_info "Total instructions: $TOTAL_INSTRUCTIONS"
    pretty_info "Total Rust instructions: $TOTAL_RUST_INSTRUCTIONS"
    pretty_info "Elapsed time: ${ELAPSED}s"
fi

if [ "$HAS_BASELINE" = false ] && [ "$SAVE_BASELINE" = false ] && [ "$QUIET_MODE" != true ]; then
    pretty_warn "No $ARCH regression baseline is available; output and Rust/Dark comparisons were still checked"
fi

# Save baseline if requested
if [ "$SAVE_BASELINE" = true ]; then
    echo "# Quick benchmark baseline - instruction counts" > "$BASELINE_FILE"
    echo "# Architecture: $ARCH" >> "$BASELINE_FILE"
    echo "# Generated: $(date -Iseconds)" >> "$BASELINE_FILE"
    echo "# Compiler: $(git -C "$PROJECT_ROOT" rev-parse --short HEAD)" >> "$BASELINE_FILE"
    for bench in $BENCHMARKS; do
        if [ -n "${RESULTS[$bench]}" ]; then
            echo "$bench=${RESULTS[$bench]}" >> "$BASELINE_FILE"
        fi
    done
    if [ "$QUIET_MODE" != true ]; then
        pretty_ok "Baseline saved to $BASELINE_FILE"
    fi
fi

# Report failures
HAS_FAILURES=false

if [ ${#BUILD_FAILURES[@]} -ne 0 ]; then
    HAS_FAILURES=true
    if [ "$QUIET_MODE" = true ]; then
        echo "build failures: ${BUILD_FAILURES[*]}"
    else
        echo ""
        pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
    fi
fi

if [ ${#FAILURES[@]} -ne 0 ]; then
    HAS_FAILURES=true
    if [ "$QUIET_MODE" = true ]; then
        echo "regressions:"
        for failure in "${FAILURES[@]}"; do
            echo "  $failure"
        done
    else
        echo ""
        pretty_fail "Regressions detected:"
        for failure in "${FAILURES[@]}"; do
            echo "  - $failure"
        done
    fi
fi

if [ "$HAS_FAILURES" = true ]; then
    exit 1
fi

if [ "$QUIET_MODE" = true ]; then
    echo "success"
else
    echo ""
    pretty_ok "No regressions detected"
fi
