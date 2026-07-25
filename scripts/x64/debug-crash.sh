#!/bin/bash
# debug-crash.sh — Diagnose codegen crashes in compiled Dark programs.
#
# Usage:
#   scripts/x64/debug-crash.sh "dark expression"
#   scripts/x64/debug-crash.sh path/to/file.dark
#   scripts/x64/debug-crash.sh "expression" --watch-func ADDR  # watch callee-saved regs
#   scripts/x64/debug-crash.sh "expression" --trace-calls       # list entry points + valgrind
#
# What it does:
#   1. Compiles the expression/file to a binary.
#   2. Runs it — if it crashes, shows crash info.
#   3. With --watch-func: sets hardware watchpoints on saved callee-saved regs.
#   4. With --trace-calls: enumerates function entries and runs valgrind.

set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN="$HERE/../run-in-container"

BIN="/tmp/debug_crash_test"
MODE="crash"
WATCH_ADDR=""
EXPR=""

usage() {
    echo "Usage: $0 'dark-expression-or-file' [--watch-func ADDR] [--trace-calls]"
}

while [ $# -gt 0 ]; do
    case "$1" in
        --watch-func)
            if [ $# -lt 2 ]; then
                usage
                exit 1
            fi
            WATCH_ADDR="$2"
            MODE="watch"
            shift 2
            ;;
        --trace-calls) MODE="trace"; shift ;;
        *) EXPR="$1"; shift ;;
    esac
done

if [ -z "$EXPR" ]; then
    usage
    exit 1
fi

echo "=== Compiling ==="
if [ -f "$EXPR" ]; then
    "$RUN" ./dark "$EXPR" -o "$BIN" 2>&1 | tail -1
else
    "$RUN" ./dark -e "$EXPR" -o "$BIN" 2>&1 | tail -1
fi

case "$MODE" in
    crash)
        echo ""
        echo "=== Running with crash analysis ==="
        "$RUN" gdb -batch \
            -ex 'run' \
            -ex 'printf "\n=== CRASH INFO ===\n"' \
            -ex 'printf "RIP=%p (crash instruction)\n", $rip' \
            -ex 'printf "RAX=%p  RBX=%p  RCX=%p  RDX=%p\n", $rax, $rbx, $rcx, $rdx' \
            -ex 'printf "RSI=%p  RDI=%p  R8=%p   R9=%p\n", $rsi, $rdi, $r8, $r9' \
            -ex 'printf "R10=%p  R11=%p  R12=%p  R13=%p\n", $r10, $r11, $r12, $r13' \
            -ex 'printf "R14=%p (heap)  R15=%p (base)\n", $r14, $r15' \
            -ex 'printf "RSP=%p  RBP=%p\n", $rsp, $rbp' \
            -ex 'printf "Heap used: %d bytes\n", (long)$r14 - (long)$r15' \
            -ex 'printf "\n=== CRASH INSTRUCTION ===\n"' \
            -ex 'x/5i $rip-8' \
            -ex 'printf "\n=== STACK (16 words) ===\n"' \
            -ex 'x/16gx $rsp' \
            -ex 'printf "\n=== BACKTRACE ===\n"' \
            -ex 'bt 15' \
            "$BIN" 2>&1 | grep -v "^warning:"
        ;;

    watch)
        echo ""
        echo "=== Watching callee-saved regs at function 0x$WATCH_ADDR ==="
        echo "Setting watchpoints on [RBP-8] (RBX), [RBP-16] (R12), [RBP-24] (R13)"
        "$RUN" gdb -batch \
            -ex "break *0x${WATCH_ADDR}" \
            -ex 'run' \
            -ex 'printf "Function entered: RBP=%p\n", $rbp' \
            -ex 'printf "Saved: RBX=[RBP-8]=%p  R12=[RBP-16]=%p  R13=[RBP-24]=%p\n", *(long*)($rbp-8), *(long*)($rbp-16), *(long*)($rbp-24)' \
            -ex 'set $watch_rbp = $rbp' \
            -ex 'watch *(long*)($watch_rbp - 8)' \
            -ex 'watch *(long*)($watch_rbp - 16)' \
            -ex 'watch *(long*)($watch_rbp - 24)' \
            -ex 'continue' \
            -ex 'printf "\n=== CORRUPTION DETECTED ===\n"' \
            -ex 'printf "RIP=%p  RSP=%p  RBP=%p\n", $rip, $rsp, $rbp' \
            -ex 'printf "Distance RSP from watched RBP: %d bytes\n", (long)$rsp - (long)$watch_rbp' \
            -ex 'printf "Current saved values: RBX=%p  R12=%p  R13=%p\n", *(long*)($watch_rbp-8), *(long*)($watch_rbp-16), *(long*)($watch_rbp-24)' \
            -ex 'x/3i $rip-8' \
            -ex 'bt 10' \
            "$BIN" 2>&1 | grep -v "^warning:"
        ;;

    trace)
        echo ""
        echo "=== Finding function entry points ==="
        "$RUN" objdump -D -M intel -b binary -m i386:x86-64 \
            --adjust-vma=0x400000 "$BIN" 2>&1 | \
            awk '/push.*rbp$/ {addr=$1; getline; if (/mov.*rbp,.*rsp/) print "func @ " addr}' | \
            head -40

        echo ""
        echo "=== Valgrind check ==="
        "$RUN" valgrind --tool=memcheck "$BIN" 2>&1 | \
            grep -E "Invalid|ERROR SUMMARY|Address"
        ;;
esac
