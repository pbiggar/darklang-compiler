#!/usr/bin/env bash
# Count dynamically executed x86_64 guest instructions with the pinned QEMU plugin.

set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "Usage: qemu_instruction_count.sh <x86_64-binary> [arguments ...]" >&2
    exit 2
fi

binary="$(realpath "$1")"
shift
qemu="/opt/dcb/qemu/qemu-x86_64"
plugin="/opt/dcb/qemu/libinsn.so"

if [[ ! -x "$binary" ]]; then
    echo "Not an executable file: $binary" >&2
    exit 2
fi
if [[ ! -x "$qemu" || ! -r "$plugin" ]]; then
    echo "Pinned QEMU x86_64 instruction counter is unavailable" >&2
    exit 1
fi
if [[ "$($qemu --version | head -n 1)" != "qemu-x86_64 version 11.1.1" ]]; then
    echo "Unexpected QEMU x86_64 instruction counter version" >&2
    exit 1
fi

exec /usr/bin/timeout --signal=KILL 20s \
    "$qemu" \
    -L /usr/x86_64-linux-gnu \
    -d plugin \
    -D /dev/stderr \
    -plugin "$plugin,inline=on" \
    "$binary" "$@"
