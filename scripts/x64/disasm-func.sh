#!/bin/bash
# disasm-func.sh — Disassemble a specific function from a compiled Dark binary.
#
# Usage:
#   scripts/x64/disasm-func.sh /path/to/binary ADDR  # disassemble function at ADDR
#   scripts/x64/disasm-func.sh /path/to/binary        # list all function entry points
#
# ADDR is a hex address (e.g., 40283a). The tool finds the function starting at
# that address and shows all instructions until the next RET.

set -e

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RUN="$HERE/../run-in-container"

BIN="$1"
ADDR="${2:-}"

if [ -z "$BIN" ]; then
    echo "Usage: $0 /path/to/binary [hex_addr]"
    exit 1
fi

disasm() {
    "$RUN" objdump -D -M intel -b binary -m i386:x86-64 --adjust-vma=0x400000 "$BIN"
}

if [ -z "$ADDR" ]; then
    echo "=== Function entry points (push rbp; mov rbp, rsp) ==="
    disasm 2>&1 | awk '
        /push.*rbp$/ {
            addr = $1; sub(/:/, "", addr);
            getline;
            if (/mov.*rbp,.*rsp/) print "0x" addr
        }
    '
else
    echo "=== Function at 0x$ADDR ==="
    disasm 2>&1 | sed -n "/^  ${ADDR}.*push.*rbp/,/c3.*ret$/p"
fi
