#!/bin/bash
# generate-review-pdfs.sh - Generate annotated code PDFs for review
#
# Uses enscript + ghostscript to create 9pt, 0.25in margin PDFs
# with syntax highlighting and line numbers.

set -euo pipefail

OUTDIR="pdfs"
mkdir -p "$OUTDIR"

# enscript options:
#   -f Courier9     = 9pt base font
#   -C              = line numbers
#   -r              = landscape (more code per line)... actually portrait is better for double-sided
#   --margins=18:18:18:18  = 0.25in margins (in points: 0.25*72=18)
#   -E              = syntax highlighting (auto-detect)
#   --header='$n|Page $% of $='  = filename and page numbers
#   -p -            = output PostScript to stdout

ENSCRIPT_OPTS="-f Courier9 -C --margins=18:18:18:18 --header='\$n|%W|Page \$% of \$='"

generate_pdf() {
    local input="$1"
    local output="$2"
    local title="$3"

    echo "  Generating: $output ($title)"

    # Create a temp file with commentary header
    local tmpfile=$(mktemp /tmp/review-XXXXXX.fs)

    # Add commentary header
    cat > "$tmpfile" << HEADER
// ============================================================================
// $title
// ============================================================================
//
HEADER

    # Add file-specific commentary
    if [ -n "${4:-}" ]; then
        echo "$4" | sed 's/^/\/\/ /' >> "$tmpfile"
        echo "//" >> "$tmpfile"
        echo "// ============================================================================" >> "$tmpfile"
        echo "" >> "$tmpfile"
    fi

    cat "$input" >> "$tmpfile"

    # Generate PostScript, then convert to PDF
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

echo "Generating x86_64 backend review PDFs..."
echo ""

# 1. Type definitions
generate_pdf "src/DarkCompiler/X86_64.fs" "01-X86_64-types.pdf" \
    "X86_64.fs - Instruction Type Definitions" \
    "Defines the x86_64 instruction set as F# discriminated unions.
Registers (RAX-R15), XMM float registers, conditions, sizes,
and ~47 instruction variants (MOV, ADD, SYSCALL, SSE2 ops, etc.).
This is the ISA abstraction layer between codegen and encoding."

# 2. Code generation (the big one)
generate_pdf "src/DarkCompiler/passes/x64/6_CodeGen.fs" "02-x64-CodeGen.pdf" \
    "x64/6_CodeGen.fs - LIR to x86_64 Translation" \
    "The main codegen pass (~3000 lines). Translates LIR instructions
to x86_64 machine instructions. Key concerns:
- Two-operand conflict resolution (dest==right clobbering)
- Register mapping: X0-X7 to RAX,RDI,RSI,RCX,R8,R9,R10,RDX
- X8-X17 all alias to R11 (scratch) - collision handling
- Heap allocation via bump pointer (R14) + free list (R15)
- String literal emission, file I/O syscalls, RC helpers"

# 3. Instruction encoding
generate_pdf "src/DarkCompiler/passes/x64/7_Encoding.fs" "03-x64-Encoding.pdf" \
    "x64/7_Encoding.fs - Instructions to Bytes" \
    "Encodes x86_64 instructions into machine code bytes.
Handles REX prefixes, ModRM/SIB bytes, displacement encoding.
Key subtleties: RSP requires SIB byte, RBP cant use mod=00,
SSE2 prefixes (F2, 66), and variable-length instruction encoding."

# 4. Label resolution
generate_pdf "src/DarkCompiler/passes/x64/7_Resolve.fs" "04-x64-Resolve.pdf" \
    "x64/7_Resolve.fs - Jump Label Resolution" \
    "Resolves symbolic labels to concrete byte offsets.
Two-pass: first encode to get sizes, then fix up relative jumps.
Much simpler than ARM64 (no PC-relative data loads needed)."

# 5. ELF binary generation
generate_pdf "src/DarkCompiler/passes/x64/8_Binary_Generation_ELF.fs" "05-x64-ELF.pdf" \
    "x64/8_Binary_Generation_ELF.fs - ELF Executable Output" \
    "Generates x86_64 Linux ELF executables. Wraps encoded machine
code into proper ELF headers with program/section headers.
Entry point, text segment, data segment for string/float pools."

# 6. Platform types and arch config
generate_pdf "src/DarkCompiler/PlatformTypes.fs" "06-PlatformTypes.pdf" \
    "PlatformTypes.fs - OS/Arch Type Definitions" \
    "Defines OS (MacOS|Linux) and Arch (ARM64|X86_64) DUs.
Separated from Platform.fs for early compile-order availability."

generate_pdf "src/DarkCompiler/ArchConfig.fs" "07-ArchConfig.pdf" \
    "ArchConfig.fs - Architecture Register Configuration" \
    "Maps each architecture to its calling convention: argument
registers, callee-saved registers, scratch registers.
ARM64: 8 int args, 8 callee-saved. x86_64: 6 int args, 3 callee-saved."

# 7. Test files
generate_pdf "src/Tests/compiler-passes/X86_64EncodingTests.fs" "08-tests-Encoding.pdf" \
    "X86_64EncodingTests.fs - Encoding Unit Tests" \
    "Exact byte verification for 11 instruction families.
36+ instruction types currently untested (see gap analysis)."

generate_pdf "src/Tests/compiler-passes/X86_64CodeGenTests.fs" "09-tests-CodeGen.pdf" \
    "X86_64CodeGenTests.fs - Codegen Integration Tests" \
    "End-to-end tests: Dark expression -> compile -> execute -> verify output."

generate_pdf "src/Tests/compiler-passes/X86_64ResolveTests.fs" "10-tests-Resolve.pdf" \
    "X86_64ResolveTests.fs - Label Resolution Tests" \
    "Tests for jump target resolution and instruction sizing."

generate_pdf "src/Tests/compiler-passes/X86_64BinaryTests.fs" "11-tests-Binary.pdf" \
    "X86_64BinaryTests.fs - ELF Binary Generation Tests" \
    "Tests that the generated ELF binaries are valid and executable."

# 12. PR description
if [ -f "PR_DESCRIPTION.md" ]; then
    generate_pdf "PR_DESCRIPTION.md" "00-PR-Description.pdf" \
        "x86_64 Backend — Pull Request Description" \
        ""
fi

echo ""
echo "Done! PDFs in $OUTDIR/:"
ls -la "$OUTDIR"/*.pdf 2>/dev/null | awk '{print "  " $NF " (" $5 " bytes)"}'

# Count total pages
total_pages=0
for f in "$OUTDIR"/*.pdf; do
    pages=$(gs -q -dNODISPLAY -c "($f) (r) file runpdfbegin pdfpagecount = quit" 2>/dev/null || echo 0)
    total_pages=$((total_pages + pages))
    echo "  $f: $pages pages"
done
echo ""
echo "Total pages: $total_pages"
