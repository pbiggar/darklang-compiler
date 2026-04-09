# Dark Compiler - Claude Code Instructions

## x86_64 Backend (branch: `x64`)

The compiler now supports both ARM64 and x86_64 targets. The backend is selected
automatically based on `Platform.detectArch()`. On x86_64 Linux, programs compile
to native ELF executables without QEMU.

### Architecture

Passes 1-5 (Parser → TypeCheck → ANF → MIR → LIR → RegisterAllocation) are
**fully shared** between architectures. Passes 6-8 are per-architecture:

| ARM64 | x86_64 | Purpose |
|-------|--------|---------|
| `ARM64.fs` | `X86_64.fs` | Instruction types |
| `6_CodeGen.fs` | `6_CodeGen_X86_64.fs` | LIR → ISA |
| `7_ARM64_Encoding.fs` | `7_X86_64_Encoding.fs` | Instructions → bytes |
| `7_ARM64_Resolve.fs` | `7_X86_64_Resolve.fs` | Label fixup |
| `8_Binary_Generation_ELF.fs` | `8_Binary_Generation_ELF_X86_64.fs` | ELF output |

### Critical x86_64 Codegen Pattern: Two-Operand Conflicts

x86_64 is two-operand: `dest = dest OP src`. LIR is three-operand: `dest = left OP right`.
When `dest == right`, `MOV dest, left` clobbers right before the operation.

**Every binary operation must check for this.** Fixes:
- Commutative ops (Add, Mul, And, Or, Xor, FAdd, FMul): swap operands
- Non-commutative ops (Sub, FSub, FDiv): use scratch/temp register
- Integer Mul with dest==right: use R11 (scratch) as temp
- Float non-commutative: use XMM15 as temp

### Register Mapping (LIR → x86_64)

```
X0→RAX  X1→RDI  X2→RSI  X3→RCX  X4→R8   X5→R9
X6→R10  X7→RDX  X8-X17→R11(scratch)
X19→RBX  X20→R12  X21���R13  (callee-saved, allocatable)
X22→R14  X23→R15  (RESERVED: heap ptr, free list base)
SP→RSP
```

**RDX is mapped to X7** (rarely used) to minimize IDIV clobber conflicts.
IDIV saves/restores RDX via the red zone `[RSP-8]`.

### Known Issues

1. **Refcounting not implemented**: Heap memory is never reclaimed. Programs that
   allocate heavily (e.g., 10K iterations creating lists) will exhaust the 512MB heap.
   This affects 2 of 4530 tests (leak_check, memReclaimBurn).

### Critical x86_64 Register Aliasing

X8-X17 all map to R11 (scratch). The register allocator must never load multiple
spilled operands into these registers simultaneously. For instructions with 3+
operands (RawSet, Msub, Madd), use SaveRegs/RestoreRegs to push/pop a non-R11
register (X3/RCX) as a safe temp when multiple operands are spilled.

### Running Commands — Use the Devcontainer

All build/test commands require .NET 10 (`dotnet`), which lives inside the
`compiler-dev` Docker container. **Always run commands via `docker exec`.**

The host repo parent directory is bind-mounted to `/workspace` inside the
container. You must pass `-w /workspace/<repo-dir-name>` to set the correct
working directory (the container's default `working_dir` is `/workspace/main`,
which is for a different worktree).

```bash
# The -w flag sets the working directory inside the container.
# Replace "darklang-compiler" with the actual repo directory name if different.
DEXEC="docker exec -w /workspace/darklang-compiler compiler-dev"

# Build and run all tests
$DEXEC ./run-tests

# Run filtered tests
$DEXEC ./run-tests --filter=x86
$DEXEC ./run-tests --filter=list --quiet

# Quick expression test
$DEXEC ./dark -r -e "2 + 3"

# Arbitrary dotnet commands
$DEXEC dotnet build --verbosity quiet
```

**Fallback order:**
1. `docker exec -w /workspace/darklang-compiler compiler-dev <cmd>` — preferred
2. Run `<cmd>` directly on host — only if .NET 10 SDK is installed locally
3. If both fail, tell the user to start the devcontainer
   (`docker compose up -d` in the repo root) or install .NET 10 on the host

### Docker Architecture Notes

- ARM64 hosts: everything native
- x86_64 hosts: compiler builds natively, ARM64 test binaries run via qemu-user-static
