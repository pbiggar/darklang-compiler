#!/bin/bash

# ralph-loop.sh - Dark mutation-to-E2E test workflow
# Iterates through SURVIVED mutations and invokes Claude Code to write E2E tests

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
RESULTS_CSV="${SCRIPT_DIR}/results/results.csv"
ADDRESSED_FILE="${SCRIPT_DIR}/results/addressed.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse command line arguments
FILE_PATTERN=""
MUTATION_TYPE=""
MUTATION_ID=""
DRY_RUN=false
MAX_COUNT=0  # 0 means unlimited

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  --file=PATTERN   Only process mutations in files matching pattern"
    echo "  --type=TYPE      Only process mutation type (ARITH_ADD, CMP_LT, etc.)"
    echo "  --id=N           Process only mutation #N (no loop)"
    echo "  --count=N        Process at most N mutations then stop"
    echo "  --dry-run        Show what would be sent to Claude without invoking"
    echo "  --help           Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                             # Loop through all unaddressed mutations"
    echo "  $0 --count=5                   # Process up to 5 mutations"
    echo "  $0 --dry-run --id=1           # Preview mutation #1"
    echo "  $0 --id=1                      # Process only mutation #1"
    echo "  $0 --type=ARITH_ADD            # Process all ARITH_ADD mutations"
    echo "  $0 --file=Parser               # Process mutations in Parser files"
    exit 1
}

for arg in "$@"; do
    case $arg in
        --file=*)
            FILE_PATTERN="${arg#*=}"
            ;;
        --type=*)
            MUTATION_TYPE="${arg#*=}"
            ;;
        --id=*)
            MUTATION_ID="${arg#*=}"
            ;;
        --dry-run)
            DRY_RUN=true
            ;;
        --count=*)
            MAX_COUNT="${arg#*=}"
            ;;
        --help)
            usage
            ;;
        *)
            echo "Unknown option: $arg"
            usage
            ;;
    esac
done

# Ensure results file exists
if [[ ! -f "$RESULTS_CSV" ]]; then
    echo -e "${RED}Error: $RESULTS_CSV not found${NC}"
    echo "Run mutation testing first to generate results."
    exit 1
fi

# Create addressed file if it doesn't exist
if [[ ! -f "$ADDRESSED_FILE" ]]; then
    touch "$ADDRESSED_FILE"
fi

# Get list of already addressed mutation IDs
get_addressed_ids() {
    if [[ -f "$ADDRESSED_FILE" ]]; then
        cat "$ADDRESSED_FILE" | grep -v '^#' | grep -v '^$' || true
    fi
}

# Mutation type mappings
get_operator_change() {
    local type="$1"
    case "$type" in
        ARITH_ADD) echo "+ -> -" ;;
        ARITH_SUB) echo "- -> +" ;;
        ARITH_MUL) echo "* -> /" ;;
        ARITH_DIV) echo "/ -> *" ;;
        CMP_LT)    echo "< -> >" ;;
        CMP_GT)    echo "> -> <" ;;
        CMP_LTE)   echo "<= -> >=" ;;
        CMP_GTE)   echo ">= -> <=" ;;
        CMP_NEQ)   echo "<> -> =" ;;
        LOGIC_AND) echo "&& -> ||" ;;
        LOGIC_OR)  echo "|| -> &&" ;;
        *)         echo "$type" ;;
    esac
}

# Find the first unaddressed SURVIVED mutation matching filters
find_mutation() {
    local addressed_ids
    addressed_ids=$(get_addressed_ids)

    # Skip header line, filter for SURVIVED
    tail -n +2 "$RESULTS_CSV" | while IFS=',' read -r id type file line result time_ms; do
        # Skip non-SURVIVED mutations
        [[ "$result" != "SURVIVED" ]] && continue

        # Check if already addressed
        if echo "$addressed_ids" | grep -qx "$id"; then
            continue
        fi

        # Apply filters
        if [[ -n "$MUTATION_ID" ]] && [[ "$id" != "$MUTATION_ID" ]]; then
            continue
        fi

        if [[ -n "$FILE_PATTERN" ]] && [[ ! "$file" =~ $FILE_PATTERN ]]; then
            continue
        fi

        if [[ -n "$MUTATION_TYPE" ]] && [[ "$type" != "$MUTATION_TYPE" ]]; then
            continue
        fi

        # Found a matching mutation
        echo "$id,$type,$file,$line,$result"
        return 0
    done
}

# Extract source context (15 lines around mutation)
get_source_context() {
    local file="$1"
    local line="$2"
    local start=$((line - 7))
    local end=$((line + 7))

    [[ $start -lt 1 ]] && start=1

    if [[ -f "$file" ]]; then
        sed -n "${start},${end}p" "$file" | nl -ba -v "$start"
    else
        echo "(Source file not found: $file)"
    fi
}

# Build the prompt for Claude Code
build_prompt() {
    local id="$1"
    local type="$2"
    local file="$3"
    local line="$4"
    local operator_change
    local source_context
    local filename

    operator_change=$(get_operator_change "$type")
    source_context=$(get_source_context "$file" "$line")
    filename=$(basename "$file")

    cat <<EOF
You are analyzing a mutation that SURVIVED testing, meaning the current test suite does not detect when this code change is made.

## Mutation Details
- Mutation ID: $id
- File: $file
- Line: $line
- Type: $type ($operator_change)

## Source Context (lines around the mutation)
\`\`\`fsharp
$source_context
\`\`\`

## Task
Write an E2E test that would FAIL if this mutation were applied (i.e., the test should detect the bug that the mutation introduces).

### Steps:
1. **Understand** what code path uses this operator and what behavior it affects
2. **Design** a test input that would produce different output with the mutation applied. Validate the test fails if the mutation is present, and passes with the mutation removed.
3. **Add** the test to the appropriate .e2e file in src/Tests/e2e/
4. **Run** ./run-tests --ai to verify your test passes with the original code
5. **Commit** your changes with message: 'Add E2E test for mutation #$id'
   - Include the .e2e file you modified
   - Include mutation/results/addressed.txt (add line: $id)

### E2E Test Format
E2E tests use the format: \`expression = expected_result\`

Examples:
\`\`\`
1 + 1 = 2
5 - 3 = 2
true && false = false
\`\`\`

### Tips for Killing This Mutation
- For $type mutations, think about edge cases where $operator_change would produce a different result
- Consider boundary values, zero cases, and negative numbers
- The test should pass normally but would fail if the operator were changed

### After Adding the Test
Mark this mutation as addressed by adding "$id" to mutation/results/addressed.txt
EOF
}

# Show stats
show_stats() {
    total_survived=$(tail -n +2 "$RESULTS_CSV" | grep ',SURVIVED,' | wc -l)
    addressed_count=$(get_addressed_ids | wc -l)
    echo ""
    echo "Stats:"
    echo "  Total SURVIVED mutations: $total_survived"
    echo "  Already addressed: $addressed_count"
    echo "  Remaining: $((total_survived - addressed_count))"
}

# Process a single mutation
process_mutation() {
    local id="$1"
    local type="$2"
    local file="$3"
    local line="$4"
    local operator_change

    operator_change=$(get_operator_change "$type")

    echo -e "${YELLOW}Selected Mutation #$id${NC}"
    echo "  Type: $type ($operator_change)"
    echo "  File: $file"
    echo "  Line: $line"
    echo ""

    # Build the prompt
    prompt=$(build_prompt "$id" "$type" "$file" "$line")

    if [[ "$DRY_RUN" == true ]]; then
        echo -e "${YELLOW}=== DRY RUN: Prompt that would be sent to Claude ===${NC}"
        echo ""
        echo "$prompt"
        echo ""
        echo -e "${YELLOW}=== End of prompt ===${NC}"
        echo ""
    else
        echo -e "${GREEN}Invoking Claude Code...${NC}"
        echo ""

        # Invoke Claude Code with the prompt
        claude --dangerously-skip-permissions --print "$prompt"

        echo ""
        echo -e "${BLUE}Claude Code session ended for mutation #$id${NC}"
    fi
}

# Main execution
main() {
    echo -e "${BLUE}=== Dark mutation-to-E2E test workflow ===${NC}"
    echo ""

    local processed=0

    while true; do
        # Check count limit
        if [[ $MAX_COUNT -gt 0 && $processed -ge $MAX_COUNT ]]; then
            echo -e "${GREEN}Reached maximum count of $MAX_COUNT mutations.${NC}"
            show_stats
            exit 0
        fi

        # Find next mutation to process
        mutation=$(find_mutation)

        if [[ -z "$mutation" ]]; then
            echo -e "${GREEN}No unaddressed SURVIVED mutations found matching your criteria.${NC}"
            show_stats
            exit 0
        fi

        # Parse mutation details
        IFS=',' read -r id type file line result <<< "$mutation"

        ((++processed))
        echo -e "${BLUE}--- Processing mutation $processed${MAX_COUNT:+ of $MAX_COUNT} ---${NC}"
        echo ""

        process_mutation "$id" "$type" "$file" "$line"

        # If specific ID requested, don't loop
        if [[ -n "$MUTATION_ID" ]]; then
            echo ""
            echo -e "${GREEN}Finished processing mutation #$MUTATION_ID${NC}"
            exit 0
        fi

        # If dry-run, only show one to avoid spam
        if [[ "$DRY_RUN" == true ]]; then
            echo "Dry-run mode: showing only first mutation. Remove --dry-run to process all."
            show_stats
            exit 0
        fi

        echo ""
        echo -e "${BLUE}=== Continuing to next mutation ===${NC}"
        echo ""
    done
}

main
