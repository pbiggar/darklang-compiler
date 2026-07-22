#!/bin/bash
# dump-lir-func.sh — Dump LIR for a specific function from a Dark expression
#
# Usage:
#   ./scripts/dump-lir-func.sh "dark expression" function_name
#   ./scripts/dump-lir-func.sh "dark expression"  # dumps all functions
#
# Examples:
#   ./scripts/dump-lir-func.sh "iter([1,2,3], 0)" iter
#   ./scripts/dump-lir-func.sh "Base64.encode(Bytes.fromList([72]))" tail_i64
#
# Shows both pre- and post-register-allocation LIR for the named function.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN="$HERE/run-in-container"
EXPR="${1:-}"
FUNC="${2:-}"

if [ -z "$EXPR" ]; then
    echo "Usage: $0 'dark-expression' [function_name]"
    exit 1
fi

TMPFILE="$(mktemp -t dark-lir-dump.XXXXXX)"
OUTFILE="$(mktemp -t dark-lir-output.XXXXXX)"

cleanup() {
    rm -f "$TMPFILE" "$OUTFILE"
}

trap cleanup EXIT

# Compile and dump LIR. Keep the captured compiler output for filtering on
# success, but print it when the dump itself fails.
if [ -f "$EXPR" ]; then
    if "$RUN" ./dark --dump-lir "$EXPR" -o "$OUTFILE" > "$TMPFILE" 2>&1; then
        compile_status=0
    else
        compile_status=$?
    fi
else
    if "$RUN" ./dark --dump-lir -e "$EXPR" -o "$OUTFILE" > "$TMPFILE" 2>&1; then
        compile_status=0
    else
        compile_status=$?
    fi
fi

if [ "$compile_status" -ne 0 ]; then
    cat "$TMPFILE"
    exit "$compile_status"
fi

if [ -z "$FUNC" ]; then
    cat "$TMPFILE"
else
    echo "=== Pre-Register-Allocation: $FUNC ==="
    # Find function before "After Register Allocation"
    awk -v func="^${FUNC}:" '
        /After Register Allocation/ {in_post=1}
        !in_post && $0 ~ func {found=1}
        found && /^$/ {found=0}
        found {print}
    ' "$TMPFILE"

    echo ""
    echo "=== Post-Register-Allocation: $FUNC ==="
    awk -v func="^${FUNC}:" '
        /After Register Allocation/ {in_post=1}
        in_post && $0 ~ func {found=1}
        found && /^$/ {found=0}
        found {print}
    ' "$TMPFILE"
fi
