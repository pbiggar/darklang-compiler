# Initiatives

## Active: x86_64 backend (branch: `x64`)

Goal: reach ARM64 test parity (4486/4530 E2E tests) then merge to main.

Current: **4529/4530 (99.98%)**. Already exceeds ARM64 baseline (4486). **1 failure remains.**

Recently fixed:
- **Leak check + string refcounting (1 test)** — Implemented leak counter via data label
  with RIP-relative addressing, string RefCountInc/Dec with sentinel detection, and
  leak report at _start exit. Fixed Print instructions to not bypass epilogue.
- **RawSet register aliasing (25 tests)** — X12/X13/X14 all map to R11 on x86_64
  but register allocator loaded spilled RawSet operands into them as if distinct.
  When both ptr and value were spilled, loading both into R11 clobbered the ptr,
  causing stores to wrong addresses (corrupt FingerTree nodes, null pointers).
  Fix: save/restore X3 (RCX) via push/pop and use as non-R11 temp for ptr.
  Fixed all elet shadowing, String.split, Dict, partial_application, crypto, benchmarks.
- **HeapStore register clobbering (7 tests)** — `HeapStore(addr, offset, StringSymbol)` clobbered
  the addr register (R11 or RCX) during inline string allocation.
- **Heap bounds checking** (commit e0069b1) — HeapAlloc/RawAlloc now check against
  512MB mmap limit and exit(1) with "Out of heap memory" instead of SIGSEGV.
- **StringConcat left operand in R8/R9** (commit 73be36a) — `loadInfo right` clobbered
  R8/R9 before `loadInfo left` could read from them.
- **INT64_MIN / -1 SIGFPE** — detect overflow before IDIV.
- **Lsl/Lsr dest==shift && src==RCX** — setBit computed bit<<bit instead of 1<<bit.
- **Lsl/Lsr dest==RCX** — shift value overwrote src already in dest register.
- **Uxtw/Uxth zero-extension** — preceding 64-bit SUB left upper bits set.
- **FileReadText/WriteText/AppendText** — implemented x86_64 syscall sequences.

### Remaining 1 failure: memReclaimBurn (list.e2e L234)

Creates 10,000×400-element lists. Each iteration builds a list, calls `List.length`,
and the list goes out of scope. Without memory reclamation, the 512MB heap exhausts
around iteration ~3,000. Fixing this requires THREE things working together:

#### Current infrastructure (already implemented, safe, 0 regressions)

1. **RawAlloc free list reuse** — Implemented in `6_CodeGen_X86_64.fs`. Before bump-
   allocating, checks the free list for the matching size class. Uses PUSH/POP'd temp
   registers that are dynamically chosen to avoid conflicts with destReg/sizeReg.
   Verified: 4529/4530 tests pass (no-op when free list is empty).

2. **TaggedList RefCountDec helper** — `generateListRefCountDecHelper` in `6_CodeGen_X86_64.fs`.
   Full iterative DFS using PUSH/POP as work stack. Handles all 5 tag types (SINGLE,
   DEEP, NODE2, NODE3, LEAF). Verified working for simple cases (`let x = [1] in 42`).

3. **TaggedList RefCountInc helper** — `generateListRefCountIncHelper` in `6_CodeGen_X86_64.fs`.
   Increments the root node's refcount only (no recursion). Verified: 0 regressions.

#### What needs to happen to enable it (the hard part)

All three of the following must be enabled TOGETHER. Enabling any subset causes failures:

**A. RawSet ownership increment** — When `RawSet(ptr, offset, value, Some(TList _))`
stores a tagged list pointer into a FingerTree node, the stored value's refcount must
be incremented. This is because the LIR emits `RefCountDec` for BOTH the root pointer
AND intermediate node pointers when variables go out of scope. Without the ownership
increment, child nodes have refcount=1 but are referenced by both a variable AND a
parent node, so Dec takes them to 0 prematurely.

ARM64 does this in `6_CodeGen.fs` lines 3198-3212 (RawSet handler). The x86_64 code
is written but disabled in `6_CodeGen_X86_64.fs` (RawSet handler, search for
"ownershipInc").

**B. TaggedList RefCountDec wiring** — The `LIR.RefCountDec(_, _, TaggedList)` handler
must save 9 caller-saved registers, MOV addrReg to RAX, CALL the helper, then restore.
Code is written but commented out in `6_CodeGen_X86_64.fs`.

**C. RawAlloc genLeakCounterInc** — Add `genLeakCounterInc ctx` to the RawAlloc bump
allocation path (NOT the free list path). Without this, the Dec helper's `leakDec`
underflows the leak counter, causing tco-refcounting tests to report huge "leaks"
values. Only add this when RefCountDec is enabled.

#### Remaining 37-test regression (unsolved)

When A+B are both enabled, 37 tests fail:
- **3 tco-refcounting tests**: "Output mismatch" — leak counter underflow (fixed by C)
- **~10 crypto tests**: SIGSEGV (exit 139) — `Crypto.sha256`, `Crypto.sha384`, etc.
  crash even on inputs that don't obviously involve lists (`Bytes.create(0)`).
  Likely cause: some stdlib function used by crypto internally creates/uses lists,
  and the ownership increment or Dec is corrupting something in those code paths.
- **~24 other tests**: Not fully characterized. Need to run with A+B+C all enabled
  and check if C fixes the tco tests, leaving only crypto + others.

**Debugging clues for the 37 failures:**
- With ownership inc disabled but Dec enabled: 225 failures (children freed prematurely)
- With ownership inc enabled and Dec enabled: 37 failures
- The ownership inc saves/restores 9 regs (PUSH/POP), CALL inc helper, restore.
  The inc helper only uses RAX, RCX, RDX, RDI (all saved). Should be register-safe.
- Crypto crash RIP shows garbled instructions (jumped to invalid code). Stack or
  return address may be corrupted. SHA-256 state values visible in registers.
- `Bytes.create(0)` doesn't obviously involve lists, yet crypto tests crash. Check
  if stdlib functions used by crypto (e.g., `Bytes.toList`, hex conversion) create
  lists internally.

**Key discovery:** The LIR for `[1]` shows:
```
RefCountInc(X20, 24, list)    // SINGLE: 1→2
RefCountDec(X20, 24, list)    // SINGLE: 2→1
RefCountDec(X21, 24, list)    // LEAF: 1→0 (WITHOUT ownership inc) or 2→1 (WITH)
Call(toDisplayString, X20)
```
Without ownership inc, LEAF is freed before `toDisplayString` reads it → crash.
With ownership inc, LEAF refcount starts at 2 (1 from alloc + 1 from store) →
Dec takes it to 1 → survives. This confirms the ownership inc is necessary.

#### Generic RefCountDec (non-list types)

Generic RefCountDec for tuples, closures, etc. also caused regressions (220 failures)
when enabled. Root cause unclear but likely:
- The refcount field offset (`payloadSize`) may not match actual struct layout for
  some object types
- Objects allocated via HeapAlloc use `sizeBytes` as the free list index, but
  RefCountDec uses `payloadSize`. These may not match: HeapAlloc(24) → index 24,
  but RefCountDec(addr, 16, Generic) → index 16. Need to verify the mapping.
- Some heap objects may not have their refcount field initialized to 1

### Diagnostic tools

Three shell scripts in `scripts/`:
- **`debug-x86-crash.sh`** — Compile & run with GDB crash analysis, callee-saved watchpoints,
  or valgrind. Usage: `./scripts/debug-x86-crash.sh "expr" [--watch-func ADDR] [--trace-calls]`
- **`dump-lir-func.sh`** — Dump pre/post-regalloc LIR for a specific function.
  Usage: `./scripts/dump-lir-func.sh "expr" function_name`
- **`disasm-func.sh`** — Disassemble a function from a compiled binary.
  Usage: `./scripts/disasm-func.sh /path/to/bin [hex_addr]`

### Approach

TDD — pick a failing E2E test, write the smallest fix, run full suite.
See CLAUDE.md for x86_64 architecture decisions and known patterns.

## Long term

- mutmut testing
- matching darklang language
- increasing code coverage
- completing benchmarks
- expanding to support full language
- support full darklang stdlib
- Json stdlib module (parsing/serialization)
- support full darklang test suite
- reimplement darklang compiler in Darklang
- reimplement test suite in Darklang
- complete Unicode string support
- add optimizations
- remove crashes
- end-to-end SSA
- SSA-based HIR (sub ANF?)
- SCCP-based HIR, MIR, and LIR optimizations
- remove non-functional idioms
- unify memory RawPtr, heap primitives, reference counting. Ensure everything is reference counted.

# Short term

- int64 assumptions
- fix indentation to not nest so deeply
- add values
