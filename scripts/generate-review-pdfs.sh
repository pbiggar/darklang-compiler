#!/bin/bash
# generate-review-pdfs.sh - Generate PDFs of all PR-reviewable files
#
# Uses enscript + ghostscript to create 9pt, 0.25in margin, grayscale PDFs.

set -euo pipefail

OUTDIR="pdfs"
mkdir -p "$OUTDIR"

generate_pdf() {
    local input="$1"
    local output="$2"
    local title="$3"
    local commentary="${4:-}"

    echo "  $output ($title)"

    local tmpfile=$(mktemp /tmp/review-XXXXXX.txt)

    if [ -n "$commentary" ]; then
        cat > "$tmpfile" << HEADER
// ============================================================================
// $title
// ============================================================================
//
HEADER
        echo "$commentary" | sed 's/^/\/\/ /' >> "$tmpfile"
        echo "//" >> "$tmpfile"
        echo "// ============================================================================" >> "$tmpfile"
        echo "" >> "$tmpfile"
    fi

    cat "$input" >> "$tmpfile"

    enscript -f Courier9 -C --margins=18:18:18:18 \
        --header="$title|%W|Page \$% of \$=" \
        -p - "$tmpfile" 2>/dev/null | \
        gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite \
           -sOutputFile="$OUTDIR/$output" \
           -dColorConversionStrategy=/Gray \
           -dProcessColorModel=/DeviceGray \
           -f - 2>/dev/null

    rm -f "$tmpfile"
}

generate_diff_pdf() {
    local filepath="$1"
    local output="$2"
    local title="$3"

    echo "  $output ($title) [diff]"

    local tmpfile=$(mktemp /tmp/review-XXXXXX.txt)
    echo "// DIFF: $filepath (changes only)" > "$tmpfile"
    echo "// ============================================================================" >> "$tmpfile"
    echo "" >> "$tmpfile"
    git diff main..HEAD -- "$filepath" >> "$tmpfile" 2>/dev/null || echo "(no diff)" >> "$tmpfile"

    enscript -f Courier9 -C --margins=18:18:18:18 \
        --header="$title (diff)|%W|Page \$% of \$=" \
        -p - "$tmpfile" 2>/dev/null | \
        gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite \
           -sOutputFile="$OUTDIR/$output" \
           -dColorConversionStrategy=/Gray \
           -dProcessColorModel=/DeviceGray \
           -f - 2>/dev/null

    rm -f "$tmpfile"
}

echo "Generating PR review PDFs..."
echo ""
echo "=== PR Description ==="
[ -f "PR_DESCRIPTION.md" ] && generate_pdf "PR_DESCRIPTION.md" "00-PR-Description.pdf" "PR Description"

echo ""
echo "=== Project docs (new/modified) ==="
generate_pdf "CLAUDE.md" "01-CLAUDE.pdf" "CLAUDE.md"
generate_pdf "TODOs.md" "02-TODOs.pdf" "TODOs.md"
generate_pdf "README.md" "03-README.pdf" "README.md"
generate_pdf "docs/x64-refcounting.md" "04-x64-refcounting.pdf" "docs/x64-refcounting.md"
generate_pdf "AGENTS.md" "05-AGENTS.pdf" "AGENTS.md"

echo ""
echo "=== New x86_64 source files ==="
generate_pdf "src/DarkCompiler/X86_64.fs" "10-X86_64-types.pdf" \
    "X86_64.fs — Instruction Type Definitions" \
    "Registers, conditions, ~47 instruction variants. ISA abstraction layer."

generate_pdf "src/DarkCompiler/passes/x64/6_CodeGen.fs" "11-x64-CodeGen.pdf" \
    "x64/6_CodeGen.fs — LIR to x86_64 Translation" \
    "Main codegen (~3000 lines). Two-operand conflicts, register mapping,
heap allocation, string literals, file I/O, RC helpers."

generate_pdf "src/DarkCompiler/passes/x64/7_Encoding.fs" "12-x64-Encoding.pdf" \
    "x64/7_Encoding.fs — Instructions to Bytes" \
    "REX prefixes, ModRM/SIB bytes, SSE2 prefixes, variable-length encoding."

generate_pdf "src/DarkCompiler/passes/x64/7_Resolve.fs" "13-x64-Resolve.pdf" \
    "x64/7_Resolve.fs — Jump Label Resolution"

generate_pdf "src/DarkCompiler/passes/x64/8_Binary_Generation_ELF.fs" "14-x64-ELF.pdf" \
    "x64/8_Binary_Generation_ELF.fs — ELF Executable Output"

generate_pdf "src/DarkCompiler/PlatformTypes.fs" "15-PlatformTypes.pdf" "PlatformTypes.fs"
generate_pdf "src/DarkCompiler/ArchConfig.fs" "16-ArchConfig.pdf" "ArchConfig.fs"

echo ""
echo "=== New test files ==="
generate_pdf "src/Tests/compiler-passes/X86_64EncodingTests.fs" "20-tests-Encoding.pdf" "X86_64EncodingTests.fs"
generate_pdf "src/Tests/compiler-passes/X86_64CodeGenTests.fs" "21-tests-CodeGen.pdf" "X86_64CodeGenTests.fs"
generate_pdf "src/Tests/compiler-passes/X86_64ResolveTests.fs" "22-tests-Resolve.pdf" "X86_64ResolveTests.fs"
generate_pdf "src/Tests/compiler-passes/X86_64BinaryTests.fs" "23-tests-Binary.pdf" "X86_64BinaryTests.fs"

echo ""
echo "=== Modified shared files (diffs) ==="
generate_diff_pdf "src/DarkCompiler/passes/5_RegisterAllocation.fs" "30-diff-RegisterAllocation.pdf" "5_RegisterAllocation.fs"
generate_diff_pdf "src/DarkCompiler/CompilerLibrary.fs" "31-diff-CompilerLibrary.pdf" "CompilerLibrary.fs"
generate_diff_pdf "src/DarkCompiler/Platform.fs" "32-diff-Platform.pdf" "Platform.fs"
generate_diff_pdf "src/DarkCompiler/Runtime.fs" "33-diff-Runtime.pdf" "Runtime.fs"
generate_diff_pdf "src/DarkCompiler/Binary_ELF.fs" "34-diff-Binary_ELF.pdf" "Binary_ELF.fs"
generate_diff_pdf "src/DarkCompiler/DarkCompiler.fsproj" "35-diff-fsproj.pdf" "DarkCompiler.fsproj"
generate_diff_pdf "src/DarkCompiler/passes/arm64/6_CodeGen.fs" "36-diff-arm64-CodeGen.pdf" "arm64/6_CodeGen.fs"
generate_diff_pdf "src/Tests/Tests.fsproj" "37-diff-Tests-fsproj.pdf" "Tests.fsproj"
generate_diff_pdf "src/Tests/compiler-passes/PhiResolutionTests.fs" "38-diff-PhiResolutionTests.pdf" "PhiResolutionTests.fs"
generate_diff_pdf "src/Tests/test-suite-tooling/TestRunner.fs" "39-diff-TestRunner.pdf" "TestRunner.fs"
generate_diff_pdf "src/Tests/e2e/floats.e2e" "40-diff-floats-e2e.pdf" "floats.e2e"

echo ""
echo "=== Infrastructure ==="
generate_pdf ".devcontainer/devcontainer.json" "50-devcontainer.pdf" "devcontainer.json"
generate_diff_pdf ".gitignore" "51-diff-gitignore.pdf" ".gitignore"
generate_diff_pdf "Dockerfile" "52-diff-Dockerfile.pdf" "Dockerfile"
generate_diff_pdf "docker-compose.yml" "53-diff-docker-compose.pdf" "docker-compose.yml"

echo ""
echo "=== Scripts ==="
generate_pdf "scripts/debug-stack.sh" "60-debug-stack.pdf" "scripts/debug-stack.sh"
generate_pdf "scripts/debug-x86-crash.sh" "61-debug-x86-crash.pdf" "scripts/debug-x86-crash.sh"
generate_pdf "scripts/disasm-func.sh" "62-disasm-func.pdf" "scripts/disasm-func.sh"
generate_pdf "scripts/dump-lir-func.sh" "63-dump-lir-func.pdf" "scripts/dump-lir-func.sh"

echo ""
echo "=== Benchmarks ==="
generate_diff_pdf "benchmarks/HISTORY.md" "70-diff-benchmarks-HISTORY.pdf" "benchmarks/HISTORY.md"
generate_pdf "benchmarks/QUICK_BASELINE.txt" "71-benchmarks-baseline.pdf" "benchmarks/QUICK_BASELINE.txt"

echo ""
echo "Done! Counting pages..."
total_pages=0
for f in "$OUTDIR"/*.pdf; do
    p=$(gs -q -dNODISPLAY -dNOSAFER -c "($f) (r) file runpdfbegin pdfpagecount = quit" 2>/dev/null || echo 0)
    total_pages=$((total_pages + p))
    printf "  %-50s %s pages\n" "$(basename $f)" "$p"
done
echo ""
echo "Total: $total_pages pages ($((( total_pages + 1) / 2)) sheets double-sided)"
