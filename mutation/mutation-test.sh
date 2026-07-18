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
COMPILER_DIR="${REPO_ROOT}/src/DarkCompiler"
RESULTS_DIR="${SCRIPT_DIR}/results"
CHECKPOINT_FILE="${RESULTS_DIR}/checkpoint.txt"
SITES_FILE="${RESULTS_DIR}/mutation_sites.txt"
RESULTS_CSV="${RESULTS_DIR}/results.csv"
REPORT_FILE="${RESULTS_DIR}/report.txt"

# Timeouts
BUILD_TIMEOUT=60
TEST_TIMEOUT=120

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
    esac
done

# Check if file should be skipped
should_skip_file() {
    local basename
    basename=$(basename "$1")
    case "$basename" in
        *Test*.fs|IRPrinter.fs|Binary.fs|Binary_ELF.fs|Binary_MachO.fs|Output.fs|Platform.fs)
            return 0 ;;
    esac
    return 1
}

# Check if line should be skipped
should_skip_line() {
    local line="$1"
    # Skip comments
    [[ "$line" =~ ^[[:space:]]*// ]] && return 0
    # Skip module/open declarations
    [[ "$line" =~ ^[[:space:]]*(module|open)[[:space:]] ]] && return 0
    # Skip type/let declarations (lines starting with type, let, |, etc.)
    [[ "$line" =~ ^[[:space:]]*(type|let|\|)[[:space:]] ]] && return 0
    # Skip lines that are just pattern match arms
    [[ "$line" =~ ^[[:space:]]*\| ]] && return 0
    # Skip lines that are primarily string output (println, printf, etc.)
    [[ "$line" =~ (println|printf|print)[[:space:]] ]] && return 0
    # Skip generic type constraints and signatures (lines with 'a 'b etc.)
    [[ "$line" =~ \'[a-z][[:space:]]*\* ]] && return 0
    return 1
}

# Find mutation sites
find_mutation_sites() {
    log_info "Discovering mutation sites..."
    mkdir -p "$RESULTS_DIR"
    : > "$SITES_FILE"

    # Get list of files
    local files
    mapfile -t files < <(find "$COMPILER_DIR" -name "*.fs" 2>/dev/null)
    local file_count=0

    for file in "${files[@]}"; do
        # Apply file pattern filter
        [[ -n "$FILE_PATTERN" && "$file" != *"$FILE_PATTERN"* ]] && continue
        should_skip_file "$file" && continue
        ((file_count++))

        # Process each mutation type with its grep pattern
        local types=() grep_patterns=()

        if [[ "$MUTATION_TYPE" == "all" || "$MUTATION_TYPE" == "arith" ]]; then
            types+=("ARITH_ADD" "ARITH_SUB" "ARITH_MUL" "ARITH_DIV")
            grep_patterns+=(" + " " - " " \* " " / ")
        fi
        if [[ "$MUTATION_TYPE" == "all" || "$MUTATION_TYPE" == "cmp" ]]; then
            types+=("CMP_LT" "CMP_GT" "CMP_LTE" "CMP_GTE" "CMP_NEQ")
            grep_patterns+=(" < " " > " " <= " " >= " " <> ")
        fi
        if [[ "$MUTATION_TYPE" == "all" || "$MUTATION_TYPE" == "logic" ]]; then
            types+=("LOGIC_AND" "LOGIC_OR")
            grep_patterns+=(" && " " || ")
        fi

        for i in "${!types[@]}"; do
            local type="${types[$i]}"
            local grep_pattern="${grep_patterns[$i]}"

            grep -n -- "$grep_pattern" "$file" 2>/dev/null | while IFS=: read -r linenum line_content; do
                [[ -z "$linenum" ]] && continue
                should_skip_line "$line_content" && continue

                # Pattern-specific filters
                # Skip lines where the operator only appears in comments
                local code_part="${line_content%%//*}"

                case "$type" in
                    ARITH_ADD)
                        # Skip if + only in comment
                        [[ "$code_part" != *" + "* ]] && continue
                        ;;
                    ARITH_SUB)
                        # Skip -> arrows and string interpolation
                        [[ "$line_content" == *"->"* ]] && continue
                        [[ "$line_content" == *'$"'* ]] && continue
                        # Skip if - only in comment
                        [[ "$code_part" != *" - "* ]] && continue
                        ;;
                    ARITH_DIV)
                        # Skip // comments (entire line is a comment-like pattern)
                        [[ "$line_content" == *"//"* ]] && continue
                        ;;
                    ARITH_MUL)
                        # Skip tuple types like (Type1 * Type2) and type annotations
                        [[ "$line_content" =~ [A-Z][a-zA-Z0-9_]*[[:space:]]\*[[:space:]][A-Z] ]] && continue
                        # Skip lines with 'of' keyword (variant type definitions)
                        [[ "$line_content" == *" of "* ]] && continue
                        # Skip if * only in comment
                        [[ "$code_part" != *" * "* ]] && continue
                        ;;
                    CMP_LT|CMP_GT)
                        # Skip generic type params like <T> or List<int>
                        [[ "$line_content" =~ \<[A-Za-z] ]] && continue
                        [[ "$line_content" =~ [A-Za-z]\> ]] && continue
                        ;;
                esac

                local display_file="${file#${REPO_ROOT}/}"
                echo "${type}:${display_file}:${linenum}"
            done
        done
    done >> "$SITES_FILE"

    local site_count
    site_count=$(wc -l < "$SITES_FILE" | tr -d ' ')
    log_info "Found ${site_count} mutation sites in ${file_count} files"
}

# Apply a mutation using portable sed
apply_sed() {
    local linenum="$1" pattern="$2" replacement="$3" file="$4"
    local tmp="${file}.sedtmp"
    sed "${linenum}s/${pattern}/${replacement}/" "$file" > "$tmp" && mv "$tmp" "$file"
}

# Apply a mutation
apply_mutation() {
    local type="$1" file="$2" linenum="$3"
    local source_file
    source_file=$(resolve_source_file "$file")
    local backup="${source_file}.mutation_backup"

    cp "$source_file" "$backup"

    case "$type" in
        ARITH_ADD) apply_sed "$linenum" " + " " - " "$source_file" ;;
        ARITH_SUB) apply_sed "$linenum" " - " " + " "$source_file" ;;
        ARITH_MUL) apply_sed "$linenum" " \* " " \/ " "$source_file" ;;
        ARITH_DIV) apply_sed "$linenum" " \/ " " * " "$source_file" ;;
        CMP_LT)    apply_sed "$linenum" " < " " > " "$source_file" ;;
        CMP_GT)    apply_sed "$linenum" " > " " < " "$source_file" ;;
        CMP_LTE)   apply_sed "$linenum" " <= " " >= " "$source_file" ;;
        CMP_GTE)   apply_sed "$linenum" " >= " " <= " "$source_file" ;;
        CMP_NEQ)   apply_sed "$linenum" " <> " " = " "$source_file" ;;
        LOGIC_AND) apply_sed "$linenum" " && " " || " "$source_file" ;;
        LOGIC_OR)  apply_sed "$linenum" " || " " \\&\\& " "$source_file" ;;
    esac
}

# Restore from backup
restore_file() {
    local file="$1"
    local source_file
    source_file=$(resolve_source_file "$file")
    local backup="${source_file}.mutation_backup"
    [[ -f "$backup" ]] && mv "$backup" "$source_file"
}

# Run tests for a mutation
run_mutation_test() {
    # Build
    if ! timeout "$BUILD_TIMEOUT" dotnet build "${SCRIPT_DIR}/../src/Tests/Tests.fsproj" \
        -c Release --nologo -v q > /dev/null 2>&1; then
        echo "BUILD_FAILURE"
        return
    fi

    # Find test executable (output goes to bin/ at project root, not src/Tests/bin/)
    local test_exe="${SCRIPT_DIR}/../bin/Tests/Release/net10.0/Tests"
    [[ ! -x "$test_exe" ]] && { echo "BUILD_FAILURE"; return; }

    # Run tests
    if timeout "$TEST_TIMEOUT" "$test_exe" > /dev/null 2>&1; then
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

        apply_mutation "$type" "$file" "$linenum"

        local start_ms=$(date +%s%3N 2>/dev/null || date +%s)
        local result=$(run_mutation_test)
        local end_ms=$(date +%s%3N 2>/dev/null || date +%s)
        local elapsed=$((end_ms - start_ms))

        restore_file "$file"

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
