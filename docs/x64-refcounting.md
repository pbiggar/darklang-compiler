# x86_64 Reference Counting Implementation

## Status

4,529/4,530 tests pass. The sole failure is `memReclaimBurn` — requires
heap memory reclamation via reference counting.

The infrastructure (RC helpers, free list reuse) is implemented but disabled.
Enabling it currently causes 37 test regressions.

## Infrastructure (implemented, safe, 0 regressions when disabled)

1. **RawAlloc free list reuse** — Before bump-allocating, checks the free
   list for a matching size class. No-op when free list is empty.

2. **TaggedList RefCountDec helper** — `generateListRefCountDecHelper` in
   `passes/x64/6_CodeGen.fs`. Full iterative DFS using PUSH/POP as work
   stack. Handles all 5 tag types (SINGLE, DEEP, NODE2, NODE3, LEAF).

3. **TaggedList RefCountInc helper** — `generateListRefCountIncHelper`.
   Increments root node refcount only (no recursion).

## What needs to happen (the hard part)

All three must be enabled TOGETHER. Enabling any subset causes failures:

**A. RawSet ownership increment** — When `RawSet(ptr, offset, value,
Some(TList _))` stores a tagged list pointer into a FingerTree node, the
stored value's refcount must be incremented. ARM64 does this in
`passes/arm64/6_CodeGen.fs` lines 3198-3212. x86_64 code is written but
disabled (search for "ownershipInc").

**B. TaggedList RefCountDec wiring** — The `LIR.RefCountDec(_, _, TaggedList)`
handler must save 9 caller-saved registers, MOV addrReg to RAX, CALL the
helper, then restore. Code is written but commented out.

**C. RawAlloc genLeakCounterInc** — Add `genLeakCounterInc ctx` to the
RawAlloc bump allocation path (NOT the free list path). Without this, the
Dec helper's `leakDec` underflows the leak counter.

## 37-test regression when A+B enabled

- **3 tco-refcounting tests**: leak counter underflow (fixed by enabling C)
- **~10 crypto tests**: SIGSEGV — `Crypto.sha256`, `Crypto.sha384`, etc.
  crash. Likely cause: stdlib functions used by crypto internally create
  lists, and the ownership increment or Dec corrupts something.
- **~24 other tests**: not fully characterized

### Debugging clues

- With ownership inc disabled but Dec enabled: 225 failures (children freed prematurely)
- With ownership inc enabled and Dec enabled: 37 failures
- Crypto crash RIP shows garbled instructions — stack or return address corruption
- `Bytes.create(0)` doesn't obviously involve lists, yet crypto tests crash.
  Check if stdlib functions (e.g., `Bytes.toList`, hex conversion) create lists.

### Key discovery

The LIR for `[1]` shows:
```
RefCountInc(X20, 24, list)    // SINGLE: 1→2
RefCountDec(X20, 24, list)    // SINGLE: 2→1
RefCountDec(X21, 24, list)    // LEAF: 1→0 (WITHOUT ownership inc) or 2→1 (WITH)
Call(toDisplayString, X20)
```
Without ownership inc, LEAF is freed before `toDisplayString` reads it.
With ownership inc, LEAF survives. Confirms ownership inc is necessary.

## Generic RefCountDec (non-list types)

Causes 220 failures when enabled. Root causes:
- `payloadSize` vs `sizeBytes` mismatch in free list indexing
- Some heap objects may not have refcount initialized to 1
- Refcount field offset may not match actual struct layout for all types

## Plan

Phase 1: Fix 37-test regression (enable A+B+C, triage crypto crashes)
Phase 2: Generic RefCountDec (fix payload/size mismatch, verify all types)
Phase 3: Unify memory management (strings, dicts, bytes, closures)

## Diagnostic tools

- `scripts/debug-x86-crash.sh` — GDB crash analysis, callee-saved watchpoints
- `scripts/dump-lir-func.sh` — dump pre/post-regalloc LIR for a function
- `scripts/disasm-func.sh` — disassemble a function from compiled binary
- `scripts/debug-stack.sh` — diagnose callee-saved register corruption
