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

set -e

DEXEC="docker exec -w /workspace/darklang-compiler compiler-dev"
EXPR="$1"
FUNC="${2:-}"

if [ -z "$EXPR" ]; then
    echo "Usage: $0 'dark-expression' [function_name]"
    exit 1
fi

TMPFILE="/tmp/lir_dump_$$.txt"

# Compile and dump LIR
if [ -f "$EXPR" ]; then
    $DEXEC ./dark --dump-lir "$EXPR" -o /dev/null 2>&1 > "$TMPFILE" || true
else
    $DEXEC ./dark --dump-lir -e "$EXPR" -o /dev/null 2>&1 > "$TMPFILE" || true
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

rm -f "$TMPFILE"
