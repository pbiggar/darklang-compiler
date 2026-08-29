#!/bin/bash
# Complete reduced-workload correctness and monotonic Dark benchmark gate.
# Usage: ./benchmarks/quick_check.sh [--smoke] [--profile=NAME] [--benchmarks=NAMES] [--fast] [--reset-dark-baseline] [--decision-json=PATH] [--build] [--quiet]
#
# Options:
#   --smoke                Compile full Dark workloads in fresh storage, run them
#                          natively, and validate their established output;
#                          never starts Cachegrind or changes a snapshot
#   --benchmarks=NAMES     Comma-separated smoke workload selection (default: profile)
#   --profile=NAME         Benchmark profile used by smoke mode (default: routine)
#   --fast                 Run the declared quick-fast projection; never advances a snapshot
#   --reset-dark-baseline  Replace the architecture's Dark snapshot from a complete quick run
#   --decision-json=PATH    Persist the machine-readable aggregate decision at PATH
#   --build                Force rebuilding benchmark binaries
#   --quiet                Print only failures and the aggregate outcome
#   --help                 Show this help

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
source "$SCRIPT_DIR/infrastructure/pretty.sh"

machine_arch() {
    case "$(uname -m)" in
        x86_64|amd64) echo "x86_64" ;;
        aarch64|arm64) echo "arm64" ;;
        *) uname -m ;;
    esac
}

show_help() {
    sed -n '/^# Usage:/,/^$/ { s/^# \{0,1\}//; p; }' "$0"
}

ARCH="$(machine_arch)"
FORCE_BUILD=false
FAST_MODE=false
QUIET_MODE=false
SMOKE_MODE=false
RESET_DARK_BASELINE=false
DECISION_OUTPUT=""
BENCHMARK_SELECTION=""
PROFILE_OVERRIDE=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --smoke) SMOKE_MODE=true; shift ;;
        --benchmarks=*)
            BENCHMARK_SELECTION="${1#*=}"
            if [ -z "$BENCHMARK_SELECTION" ]; then
                pretty_fail "--benchmarks requires a non-empty comma-separated list"
                exit 1
            fi
            shift
            ;;
        --profile=*)
            PROFILE_OVERRIDE="${1#*=}"
            if [ -z "$PROFILE_OVERRIDE" ]; then
                pretty_fail "--profile requires a non-empty name"
                exit 1
            fi
            shift
            ;;
        --fast) FAST_MODE=true; shift ;;
        --reset-dark-baseline) RESET_DARK_BASELINE=true; shift ;;
        --decision-json=*)
            DECISION_OUTPUT="${1#*=}"
            if [ -z "$DECISION_OUTPUT" ]; then
                pretty_fail "--decision-json requires a non-empty path"
                exit 1
            fi
            shift
            ;;
        --build) FORCE_BUILD=true; shift ;;
        --quiet) QUIET_MODE=true; shift ;;
        --help|-h) show_help; exit 0 ;;
        *)
            echo "Unknown option: $1"
            show_help
            exit 1
            ;;
    esac
done

if [ "$FAST_MODE" = true ] && [ "$RESET_DARK_BASELINE" = true ]; then
    pretty_fail "--reset-dark-baseline requires the complete quick profile; --fast is not eligible"
    exit 1
fi

if [ "$SMOKE_MODE" = true ] && { [ "$FAST_MODE" = true ] || [ "$RESET_DARK_BASELINE" = true ] || [ -n "$DECISION_OUTPUT" ]; }; then
    pretty_fail "--smoke cannot reset or compare benchmark snapshots"
    exit 1
fi

if [ "$SMOKE_MODE" != true ] && ! command -v valgrind &> /dev/null; then
    pretty_fail "valgrind is not installed"
    exit 1
fi
if [ "$SMOKE_MODE" != true ] && ! command -v rustc &> /dev/null; then
    pretty_fail "rustc is not installed"
    exit 1
fi

PROFILE="quick"
if [ "$FAST_MODE" = true ]; then
    PROFILE="quick-fast"
fi
if [ "$SMOKE_MODE" = true ]; then
    PROFILE="${PROFILE_OVERRIDE:-routine}"
elif [ -n "$PROFILE_OVERRIDE" ]; then
    pretty_fail "--profile is supported only with --smoke"
    exit 1
fi
if ! BENCHMARKS=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_profiles.py" "$PROFILE"); then
    exit 1
fi
if [ -n "$BENCHMARK_SELECTION" ]; then
    BENCHMARKS="${BENCHMARK_SELECTION//,/ }"
fi
if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" check-profile "$PROFILE"; then
    pretty_fail "Benchmark parity contract failed"
    exit 1
fi

# Fail before expensive work when normal comparison cannot enforce the contract.
if [ "$SMOKE_MODE" != true ] && [ "$RESET_DARK_BASELINE" = false ]; then
    if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_baseline.py" validate \
        --benchmarks-dir "$SCRIPT_DIR" --architecture "$ARCH" --profile quick; then
        exit 1
    fi
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

TEMP_DIR="$(mktemp -d)"
COUNTS_FILE="$TEMP_DIR/dark-counts.tsv"
DECISION_FILE="$TEMP_DIR/dark-suite-decision.json"
if [ -n "$DECISION_OUTPUT" ]; then
    DECISION_FILE="$DECISION_OUTPUT"
fi
GENERATED_RUST_BINARIES=()

clean_generated_files() {
    rm -f "$PROJECT_ROOT"/cachegrind.out.*
    if [ "${#GENERATED_RUST_BINARIES[@]}" -gt 0 ]; then
        rm -f -- "${GENERATED_RUST_BINARIES[@]}"
    fi
    rm -rf "$TEMP_DIR"
}
trap clean_generated_files EXIT
rm -f "$PROJECT_ROOT"/cachegrind.out.*

FAILURES=()
BUILD_FAILURES=()
TOTAL_INSTRUCTIONS=0
TOTAL_RUST_INSTRUCTIONS=0
START_TIME=$(date +%s)

concise_failure() {
    local output="$1"
    printf '%s\n' "$output" \
        | sed '/^[[:space:]]*$/d' \
        | tail -n 1 \
        | tr '\n' ' ' \
        | cut -c1-300
}

if [ "$SMOKE_MODE" = true ]; then
    PASSED=0
    for bench in $BENCHMARKS; do
        PROBLEM_DIR="$SCRIPT_DIR/problems/$bench"
        MAIN_SOURCE="$PROBLEM_DIR/dark/main.dark"
        EXPECTED_FILE="$PROBLEM_DIR/expected_output.txt"
        if [ -f "$PROBLEM_DIR/dark/expected_output.txt" ]; then
            EXPECTED_FILE="$PROBLEM_DIR/dark/expected_output.txt"
        fi
        MAIN_BINARY="$TEMP_DIR/${bench}-main"

        if [ ! -f "$MAIN_SOURCE" ] || [ ! -f "$EXPECTED_FILE" ]; then
            FAILURES+=("metadata|$bench|missing dark/main.dark or expected output")
            continue
        fi

        if ! MAIN_BUILD_OUTPUT=$("$PROJECT_ROOT/dark" --allow-internal "$MAIN_SOURCE" -o "$MAIN_BINARY" -q 2>&1); then
            BUILD_FAILURES+=("main|$bench|$(concise_failure "$MAIN_BUILD_OUTPUT")")
            continue
        fi
        EXPECTED_OUTPUT=$(<"$EXPECTED_FILE")
        set +e
        MAIN_OUTPUT=$(timeout 120 "$MAIN_BINARY" 2>&1)
        MAIN_EXIT=$?
        set -e
        if [ "$MAIN_EXIT" -eq 124 ]; then
            FAILURES+=("timeout|$bench|native workload exceeded 120 seconds")
        elif [ "$MAIN_EXIT" -ne 0 ]; then
            FAILURES+=("execute|$bench|exit $MAIN_EXIT: $(concise_failure "$MAIN_OUTPUT")")
        elif [ "$MAIN_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
            FAILURES+=("output|$bench|got '$(printf '%s' "$MAIN_OUTPUT" | cut -c1-120)', expected '$(printf '%s' "$EXPECTED_OUTPUT" | cut -c1-120)'")
        else
            PASSED=$((PASSED + 1))
        fi
    done

    for failure in "${BUILD_FAILURES[@]}"; do
        IFS='|' read -r variant workload detail <<< "$failure"
        printf 'SMOKE_FAILURE phase=compile-%s workload=%s detail=%s\n' "$variant" "$workload" "$detail"
    done
    for failure in "${FAILURES[@]}"; do
        IFS='|' read -r phase workload detail <<< "$failure"
        printf 'SMOKE_FAILURE phase=%s workload=%s detail=%s\n' "$phase" "$workload" "$detail"
    done
    FAILED=$((${#BUILD_FAILURES[@]} + ${#FAILURES[@]}))
    ELAPSED=$(($(date +%s) - START_TIME))
    printf 'SMOKE_SUMMARY passed=%s failed=%s elapsed_seconds=%s\n' "$PASSED" "$FAILED" "$ELAPSED"
    if [ "$FAILED" -ne 0 ]; then
        exit 1
    fi
    exit 0
fi

if [ "$QUIET_MODE" != true ]; then
    pretty_section "Quick benchmark check ($PROFILE profile)"
    pretty_info "Architecture: $ARCH"
    if [ "$RESET_DARK_BASELINE" = true ]; then
        pretty_warn "Dark baseline reset requested; only this complete successful run is eligible"
    fi
    echo ""
fi

for bench in $BENCHMARKS; do
    PROBLEM_DIR="$SCRIPT_DIR/problems/$bench"
    QUICK_DARK="$PROBLEM_DIR/dark/quick.dark"
    QUICK_BIN="$PROBLEM_DIR/dark/quick"
    QUICK_RUST="$PROBLEM_DIR/rust/quick.rs"
    QUICK_RUST_BIN="$PROBLEM_DIR/rust/quick"
    QUICK_EXPECTED="$PROBLEM_DIR/quick_expected_output.txt"
    GENERATED_RUST_BINARIES+=("$QUICK_RUST_BIN")

    if ! QUICK_PARITY_STATUS=$(python3 "$SCRIPT_DIR/infrastructure/benchmark_parity.py" status "$bench" quick); then
        FAILURES+=("$bench: quick parity status unavailable")
        continue
    fi

    NEEDS_BUILD=false
    if [ "$FORCE_BUILD" = true ] || [ ! -x "$QUICK_BIN" ] || [ "$QUICK_DARK" -nt "$QUICK_BIN" ] || [ "$COMPILER_DLL" -nt "$QUICK_BIN" ]; then
        NEEDS_BUILD=true
    fi
    if [ "$NEEDS_BUILD" = true ] && ! "$PROJECT_ROOT/dark" --allow-internal "$QUICK_DARK" -o "$QUICK_BIN" -q 2>/dev/null; then
        BUILD_FAILURES+=("$bench")
        continue
    fi

    RUST_NEEDS_BUILD=false
    if [ "$FORCE_BUILD" = true ] || [ ! -x "$QUICK_RUST_BIN" ] || [ "$QUICK_RUST" -nt "$QUICK_RUST_BIN" ]; then
        RUST_NEEDS_BUILD=true
    fi
    if [ "$RUST_NEEDS_BUILD" = true ] && ! rustc -C opt-level=3 "$QUICK_RUST" -o "$QUICK_RUST_BIN" 2>/dev/null; then
        BUILD_FAILURES+=("$bench/rust")
        continue
    fi

    if [ ! -f "$QUICK_EXPECTED" ]; then
        FAILURES+=("$bench: missing quick_expected_output.txt")
        continue
    fi
    EXPECTED_OUTPUT=$(<"$QUICK_EXPECTED")
    if ! DARK_OUTPUT=$("$QUICK_BIN"); then
        FAILURES+=("$bench: Dark quick execution failed")
        continue
    fi
    if ! RUST_OUTPUT=$("$QUICK_RUST_BIN"); then
        FAILURES+=("$bench: Rust quick execution failed")
        continue
    fi
    if [ "$DARK_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
        FAILURES+=("$bench: Dark output mismatch")
        continue
    fi
    if [ "$RUST_OUTPUT" != "$EXPECTED_OUTPUT" ]; then
        FAILURES+=("$bench: Rust output mismatch")
        continue
    fi

    CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no "$QUICK_BIN" 2>&1 || true)
    I_REFS=$(printf '%s\n' "$CG_OUTPUT" | sed -n 's/.*I refs:[[:space:]]*//p' | tr -d ',')
    if [[ ! "$I_REFS" =~ ^[1-9][0-9]*$ ]]; then
        FAILURES+=("$bench: Dark Cachegrind did not produce exactly one positive instruction count")
        continue
    fi

    RUST_CG_OUTPUT=$(valgrind --tool=cachegrind --cache-sim=no --branch-sim=no "$QUICK_RUST_BIN" 2>&1 || true)
    RUST_I_REFS=$(printf '%s\n' "$RUST_CG_OUTPUT" | sed -n 's/.*I refs:[[:space:]]*//p' | tr -d ',')
    if [[ ! "$RUST_I_REFS" =~ ^[1-9][0-9]*$ ]]; then
        FAILURES+=("$bench: Rust Cachegrind did not produce exactly one positive instruction count")
        continue
    fi

    printf '%s\t%s\n' "$bench" "$I_REFS" >> "$COUNTS_FILE"
    TOTAL_INSTRUCTIONS=$((TOTAL_INSTRUCTIONS + I_REFS))
    TOTAL_RUST_INSTRUCTIONS=$((TOTAL_RUST_INSTRUCTIONS + RUST_I_REFS))
    if [ "$QUIET_MODE" != true ]; then
        if [ "$QUICK_PARITY_STATUS" = "comparable" ]; then
            RATIO=$(awk -v dark="$I_REFS" -v rust="$RUST_I_REFS" 'BEGIN { printf "%.2f", dark / rust }')
            pretty_info "$bench: Dark $I_REFS; Rust $RUST_I_REFS (Dark/Rust ${RATIO}x)"
        else
            pretty_info "$bench: Dark $I_REFS; Rust $RUST_I_REFS (Rust diagnostic: $QUICK_PARITY_STATUS)"
        fi
    fi
done

if [ "${#BUILD_FAILURES[@]}" -ne 0 ] || [ "${#FAILURES[@]}" -ne 0 ]; then
    if [ "${#BUILD_FAILURES[@]}" -ne 0 ]; then
        pretty_fail "Build failures: ${BUILD_FAILURES[*]}"
    fi
    for failure in "${FAILURES[@]}"; do
        pretty_fail "$failure"
    done
    pretty_fail "Incomplete quick run; the Dark snapshot was not compared or changed"
    exit 1
fi

COMMIT=$(git -C "$PROJECT_ROOT" rev-parse HEAD)
SUBJECT=$(git -C "$PROJECT_ROOT" log -1 --format=%s)
TIMESTAMP=$(date -u -Iseconds)
BASELINE_ARGS=()
if [ "$FAST_MODE" = true ]; then
    BASELINE_ARGS+=(--fast)
fi
if [ "$RESET_DARK_BASELINE" = true ]; then
    BASELINE_ARGS+=(--reset)
fi
if [ "$QUIET_MODE" = true ]; then
    BASELINE_ARGS+=(--quiet)
fi

if ! python3 "$SCRIPT_DIR/infrastructure/benchmark_baseline.py" quick \
    --benchmarks-dir "$SCRIPT_DIR" \
    --architecture "$ARCH" \
    --counts "$COUNTS_FILE" \
    --commit "$COMMIT" \
    --subject "$SUBJECT" \
    --timestamp "$TIMESTAMP" \
    --decision-json "$DECISION_FILE" \
    "${BASELINE_ARGS[@]}"; then
    exit 1
fi

END_TIME=$(date +%s)
if [ "$QUIET_MODE" != true ]; then
    pretty_info "Total Dark instructions: $TOTAL_INSTRUCTIONS"
    pretty_info "Total Rust instructions: $TOTAL_RUST_INSTRUCTIONS"
    pretty_info "Elapsed time: $((END_TIME - START_TIME))s"
fi
pretty_ok "Quick benchmark suite passed"
