#!/bin/bash
# Show worktree status with merge info relative to main

# Colors
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RED='\033[0;31m'
CYAN='\033[0;36m'
DIM='\033[0;90m'
NC='\033[0m' # No Color

printf "%-12s %s\n" "BRANCH" "STATUS"
printf "%-12s %s\n" "------" "------"

# Collect output, then sort by branch name
{
git worktree list | while read -r line; do
    path=$(echo "$line" | awk '{print $1}')

    # Extract branch, handling prunable entries
    if echo "$line" | grep -q "prunable"; then
        branch=$(echo "$line" | sed 's/.*\[\(.*\)\].*/\1/' | sed 's/ prunable//')
        prunable=" ${DIM}(prunable)${NC}"
    else
        branch=$(echo "$line" | sed 's/.*\[\(.*\)\].*/\1/')
        prunable=""
    fi

    if [ "$branch" = "main" ]; then
        echo -e "main\t${CYAN}main${NC}\t${GREEN}✓ base${NC}"
        continue
    fi

    # Check if worktree path exists and is accessible
    if [ -d "$path" ] && git -C "$path" rev-parse HEAD &>/dev/null; then
        ahead=$(git -C "$path" rev-list --count main..HEAD 2>/dev/null || echo "?")
        behind=$(git -C "$path" rev-list --count HEAD..main 2>/dev/null || echo "?")

        if [ "$ahead" = "0" ] && [ "$behind" = "0" ]; then
            status="${GREEN}✓ merged${NC}${prunable}"
        elif [ "$ahead" = "0" ]; then
            status="${YELLOW}↓ ${behind} behind${NC}${prunable}"
        elif [ "$behind" = "0" ]; then
            status="${RED}↑ ${ahead} ahead${NC}${prunable}"
        else
            status="${RED}↑ ${ahead} ahead, ↓ ${behind} behind${NC}${prunable}"
        fi
    else
        status="${DIM}(inaccessible)${NC}${prunable}"
    fi

    echo -e "${branch}\t${branch}\t${status}"
done
} | sort -t$'\t' -k1 | while IFS=$'\t' read -r _ branch status; do
    printf "%-12s $status\n" "$branch"
done
