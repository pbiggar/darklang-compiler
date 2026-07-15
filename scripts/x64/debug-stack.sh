#!/bin/bash
# debug-stack.sh — Diagnose callee-saved register corruption in compiled Dark programs.
#
# Usage: scripts/x64/debug-stack.sh "expression" [function_addr_hex]
# Example: scripts/x64/debug-stack.sh 'iter([1,2,3,4,5,6,7,8,9], 2)' 40283a
#
# What it does:
#  1. Compiles the expression to a binary.
#  2. Finds all function entry points (push rbp).
#  3. Sets GDB watchpoints on the callee-saved register save locations.
#  4. Reports which function corrupts the saved registers.
#
# Requires gdb and objdump — run via scripts/run-in-container (auto-detects
# the devcontainer if invoked from the host).

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN="$HERE/../run-in-container"

EXPR="${1:-}"
WATCH_FUNC="${2:-}"

if [ -z "$EXPR" ]; then
    echo "Usage: $0 'dark-expression-or-file' [function_addr_hex]"
    exit 1
fi

BINPATH="/tmp/debug_stack_test"
rm -f "$BINPATH"

if [ -f "$EXPR" ]; then
    echo "=== Compiling file: $EXPR ==="
    "$RUN" ./dark "$EXPR" -o "$BINPATH" 2>&1 | tail -1
else
    echo "=== Compiling expression ==="
    "$RUN" ./dark -e "$EXPR" -o "$BINPATH" 2>&1 | tail -1
fi

echo ""
echo "=== Finding function entry points ==="
"$RUN" objdump -D -M intel -b binary -m i386:x86-64 --adjust-vma=0x400000 "$BINPATH" 2>&1 | \
    grep "push.*rbp$" | awk '{print $1}' | sed 's/://' | while read addr; do
    echo "  Function at 0x$addr"
done

echo ""
echo "=== Running with GDB crash analysis ==="
"$RUN" gdb -batch \
    -ex 'run' \
    -ex 'printf "CRASH at RIP=%p\n", $rip' \
    -ex 'printf "Registers: RAX=%p RBX=%p R12=%p R13=%p R14=%p R15=%p\n", $rax, $rbx, $r12, $r13, $r14, $r15' \
    -ex 'printf "Stack: RSP=%p RBP=%p\n", $rsp, $rbp' \
    -ex 'x/5i $rip-8' \
    -ex 'x/16gx $rsp' \
    "$BINPATH" 2>&1 | grep -v "^warning:" | tail -30

echo ""
echo "=== Checking for callee-saved register corruption ==="
if [ -n "$WATCH_FUNC" ]; then
    echo "Watching function at 0x$WATCH_FUNC for callee-saved corruption..."

    # Entry prologue: push rbp; mov rbp,rsp; push rbx; push r12; push r13
    # After prologue: [RBP-8]=RBX, [RBP-16]=R12, [RBP-24]=R13

    "$RUN" gdb -batch \
        -ex "break *0x${WATCH_FUNC}" \
        -ex 'run' \
        -ex 'finish' \
        -ex 'printf "After prologue: RBP=%p\n", $rbp' \
        -ex 'printf "Saved RBX=[RBP-8]=%p  R12=[RBP-16]=%p  R13=[RBP-24]=%p\n", *(long*)($rbp-8), *(long*)($rbp-16), *(long*)($rbp-24)' \
        -ex 'watch *(long*)($rbp-8)' \
        -ex 'watch *(long*)($rbp-16)' \
        -ex 'watch *(long*)($rbp-24)' \
        -ex 'continue' \
        -ex 'printf "\nWATCHPOINT: RIP=%p\n", $rip' \
        -ex 'printf "Saved RBX=[RBP-8]=%p  R12=[RBP-16]=%p  R13=[RBP-24]=%p\n", *(long*)($rbp-8), *(long*)($rbp-16), *(long*)($rbp-24)' \
        -ex 'x/3i $rip-5' \
        -ex 'bt 5' \
        "$BINPATH" 2>&1 | grep -v "^warning:"
fi
