#!/bin/bash
# mutation-test.sh - Mutation Testing for the Dark compiler
#
# Usage: ./mutation-test.sh [OPTIONS]
#   --file=PATTERN    Only mutate files matching PATTERN
#   --type=TYPE       Only apply mutation type (arith|cmp|logic|all)
#   --resume          Resume from checkpoint
#   --dry-run         Show mutations without executing
#   --limit=N         Stop after N mutations
#   -h, --help        Show this help message

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
SITE_HELPER="${SCRIPT_DIR}/ocaml_sites.py"
RESULTS_DIR="${SCRIPT_DIR}/results"
CHECKPOINT_FILE="${RESULTS_DIR}/checkpoint.txt"
SITES_FILE="${RESULTS_DIR}/mutation_sites.txt"
RESULTS_CSV="${RESULTS_DIR}/results.csv"
REPORT_FILE="${RESULTS_DIR}/report.txt"

# Timeouts
BUILD_TIMEOUT=60
TEST_TIMEOUT=600

# Counters
TOTAL_MUTATIONS=0
KILLED_MUTATIONS=0
SURVIVED_MUTATIONS=0
BUILD_FAILURES=0
TIMEOUT_MUTATIONS=0

# Options
FILE_PATTERN=""
MUTATION_TYPE="all"
RESUME=false
DRY_RUN=false
LIMIT=0

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}[INFO]${NC} $1"; }
log_success() { echo -e "${GREEN}[PASS]${NC} $1"; }
log_fail() { echo -e "${RED}[FAIL]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }

resolve_source_file() {
    local file="$1"
    case "$file" in
        /*) echo "$file" ;;
        *) echo "${REPO_ROOT}/${file}" ;;
    esac
}

# Parse arguments
for arg in "$@"; do
    case $arg in
        --file=*) FILE_PATTERN="${arg#*=}" ;;
        --type=*) MUTATION_TYPE="${arg#*=}" ;;
        --resume) RESUME=true ;;
        --dry-run) DRY_RUN=true ;;
        --limit=*) LIMIT="${arg#*=}" ;;
        -h|--help)
            head -12 "$0" | tail -10
            exit 0
            ;;
        *) echo "Unknown option: $arg" >&2; exit 1 ;;
    esac
done

case "$MUTATION_TYPE" in arith|cmp|logic|all) ;; *) echo "Invalid mutation type: $MUTATION_TYPE" >&2; exit 1 ;; esac
[[ "$LIMIT" =~ ^[0-9]+$ ]] || { echo "--limit must be a nonnegative integer" >&2; exit 1; }

# Discovery and application share an OCaml lexical scanner. Empty selections
# must fail rather than report a successful mutation run.
find_mutation_sites() {
    log_info "Discovering OCaml mutation sites..."
    mkdir -p "$RESULTS_DIR"
    if ! python3 "$SITE_HELPER" discover "$REPO_ROOT" \
        --file "$FILE_PATTERN" --type "$MUTATION_TYPE" > "$SITES_FILE"; then
        rm -f "$SITES_FILE"
        exit 1
    fi
    log_info "Found $(wc -l < "$SITES_FILE") mutation sites"
}

ACTIVE_FILE=""
apply_mutation() {
    local type="$1" file="$2" linenum="$3"
    local source_file backup
    source_file=$(resolve_source_file "$file")
    backup="${source_file}.mutation_backup"
    if [[ -e "$backup" ]]; then
        echo "Refusing to overwrite existing mutation backup: $backup" >&2
        return 1
    fi
    cp "$source_file" "$backup" || return 1
    ACTIVE_FILE="$file"
    python3 "$SITE_HELPER" apply "$type" "$source_file" "$linenum"
}

restore_file() {
    local source_file backup
    source_file=$(resolve_source_file "$1")
    backup="${source_file}.mutation_backup"
    [[ -f "$backup" ]] && mv "$backup" "$source_file"
}

# Restore source when a mutation or its verification is interrupted.
trap '[[ -z "$ACTIVE_FILE" ]] || restore_file "$ACTIVE_FILE"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# Run tests for a mutation
run_mutation_test() {
    # Build
    if ! timeout "$BUILD_TIMEOUT" "${REPO_ROOT}/build" --ai > /dev/null 2>&1; then
        echo "BUILD_FAILURE"
        return
    fi

    # Execute the already-built native runner.
    [[ ! -x "${REPO_ROOT}/_build/default/test/tests_main.exe" ]] && { echo "BUILD_FAILURE"; return; }

    # Run tests
    if timeout "$TEST_TIMEOUT" "${REPO_ROOT}/run-tests" --quiet > /dev/null 2>&1; then
        echo "SURVIVED"
    else
        local rc=$?
        [[ $rc -eq 124 ]] && echo "TIMEOUT" || echo "KILLED"
    fi
}

# Generate report
generate_report() {
    log_info "Generating report..."

    local effective=$((KILLED_MUTATIONS + SURVIVED_MUTATIONS))
    local score=0
    if [[ $effective -gt 0 ]]; then
        # Use awk instead of bc for portability
        score=$(awk "BEGIN {printf \"%.1f\", $KILLED_MUTATIONS * 100 / $effective}")
    fi

    cat << EOF > "$REPORT_FILE"
================================================================================
MUTATION TESTING REPORT - $(date)
================================================================================

SUMMARY
-------
Total Mutations:    $TOTAL_MUTATIONS
Killed:             $KILLED_MUTATIONS
Survived:           $SURVIVED_MUTATIONS
Build Failures:     $BUILD_FAILURES
Timeouts:           $TIMEOUT_MUTATIONS

MUTATION SCORE:     ${score}%

SURVIVED MUTATIONS
------------------
EOF

    grep ",SURVIVED," "$RESULTS_CSV" 2>/dev/null | while IFS=, read -r id type file line result time; do
        echo "  [$type] $(basename "$file"):${line}"
    done >> "$REPORT_FILE"

    echo ""
    echo "==============================================="
    echo "MUTATION TESTING COMPLETE"
    echo "==============================================="
    echo "Total:      $TOTAL_MUTATIONS"
    echo "Killed:     $KILLED_MUTATIONS"
    echo "Survived:   $SURVIVED_MUTATIONS"
    echo "Build Fail: $BUILD_FAILURES"
    echo "Timeouts:   $TIMEOUT_MUTATIONS"
    echo "MUTATION SCORE: ${score}%"
    echo "==============================================="
    echo "Report: ${REPORT_FILE}"
}

# Main
main() {
    log_info "Dark Compiler Mutation Testing"
    log_info "================================"

    mkdir -p "$RESULTS_DIR"

    # Discover sites
    if [[ ! -f "$SITES_FILE" ]] || [[ "$RESUME" != true ]]; then
        find_mutation_sites
    else
        log_info "Using cached sites"
    fi

    local total_sites
    total_sites=$(wc -l < "$SITES_FILE" | tr -d ' ')
    if [[ "$total_sites" -eq 0 ]]; then
        echo "No mutation sites selected; rediscover without --resume." >&2
        exit 1
    fi
    log_info "Total sites: ${total_sites}"

    # Dry run
    if [[ "$DRY_RUN" == true ]]; then
        log_info "Dry run - showing first 50 mutations:"
        head -50 "$SITES_FILE" | while IFS=: read -r type file linenum; do
            local line
            local source_file
            source_file=$(resolve_source_file "$file")
            line=$(sed -n "${linenum}p" "$source_file" 2>/dev/null | head -c 80)
            echo "  [$type] $(basename "$file"):${linenum}"
            echo "    ${line}..."
        done
        log_info "Total: ${total_sites} mutation sites"
        exit 0
    fi

    # Initialize CSV
    if [[ "$RESUME" != true ]] || [[ ! -f "$RESULTS_CSV" ]]; then
        echo "mutation_id,type,file,line,result,time_ms" > "$RESULTS_CSV"
    fi

    # Get checkpoint
    local start_line=1
    if [[ "$RESUME" == true ]] && [[ -f "$CHECKPOINT_FILE" ]]; then
        start_line=$(cat "$CHECKPOINT_FILE")
        log_info "Resuming from #${start_line}"
    fi

    local mutation_id=0
    while IFS=: read -r type file linenum; do
        ((mutation_id++))
        [[ $mutation_id -lt $start_line ]] && continue
        [[ $LIMIT -gt 0 && $TOTAL_MUTATIONS -ge $LIMIT ]] && break

        echo "$mutation_id" > "$CHECKPOINT_FILE"
        echo -n "[${mutation_id}/${total_sites}] ${type} $(basename "$file"):${linenum} ... "

        apply_mutation "$type" "$file" "$linenum" || exit 1

        local start_ms=$(date +%s%3N 2>/dev/null || date +%s)
        local result=$(run_mutation_test)
        local end_ms=$(date +%s%3N 2>/dev/null || date +%s)
        local elapsed=$((end_ms - start_ms))

        restore_file "$file"
        ACTIVE_FILE=""

        echo "${mutation_id},${type},${file},${linenum},${result},${elapsed}" >> "$RESULTS_CSV"

        ((TOTAL_MUTATIONS++))
        case "$result" in
            KILLED)        ((KILLED_MUTATIONS++)); log_success "KILLED (${elapsed}ms)" ;;
            SURVIVED)      ((SURVIVED_MUTATIONS++)); log_fail "SURVIVED (${elapsed}ms)" ;;
            BUILD_FAILURE) ((BUILD_FAILURES++)); log_warn "BUILD_FAILURE" ;;
            TIMEOUT)       ((TIMEOUT_MUTATIONS++)); log_warn "TIMEOUT" ;;
        esac
    done < "$SITES_FILE"

    generate_report
}

main
