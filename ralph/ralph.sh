#!/bin/bash
# ralph.sh - Run a playbook
# Usage: ./ralph.sh <playbook> [--iterations=N] [--dry-run]

set -euo pipefail

usage() {
    echo "Usage: ./ralph.sh <playbook> [--iterations=N] [--dry-run]" >&2
}

if [[ $# -lt 1 ]]; then
    usage
    exit 64
fi

playbook="$1"; shift
iterations=1
dry_run=false

for arg in "$@"; do
    case $arg in
        --iterations=*) iterations="${arg#*=}" ;;
        --dry-run) dry_run=true ;;
    esac
done

prompt=$(cat "$playbook")

for ((i=1; i<=iterations; i++)); do
    if [[ "$dry_run" == true ]]; then
        echo "=== Would send to Codex ==="
        echo "$prompt"
        exit 0
    fi

    echo "=== Iteration $i of $iterations ==="
    codex exec --dangerously-bypass-approvals-and-sandbox "$prompt"
done
