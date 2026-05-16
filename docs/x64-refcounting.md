# x86_64 Reference Counting Implementation

## Status

The x64 fixed-block and tagged-list root RC paths are enabled. The known
remaining memory work is recursive payload release for fixed blocks/lists and
dict/HAMT reclamation.

## Infrastructure

1. **RawAlloc free list reuse** — Before bump-allocating, checks the free
   list for a matching size class. Leak accounting increments only for bump
   allocations.

2. **TaggedList RefCountDec helper** — `generateListRefCountDecHelper` in
   `passes/x64/6_CodeGen.fs`. Full iterative DFS using PUSH/POP as work
   stack. Handles all 5 tag types (SINGLE, DEEP, NODE2, NODE3, LEAF). Wired
   through `LIR.RefCountDec(_, _, TaggedList)`.

3. **TaggedList RefCountInc helper** — `generateListRefCountIncHelper`.
   Increments root node refcount only (no recursion). Wired through
   `LIR.RefCountInc(_, _, TaggedList)`.

4. **RawSet list edge retains** — `RawSet(ptr, offset, value, Some(TList _))`
   retains the stored tagged-list pointer because the parent node now owns that
   edge.

## Enabled x64 List Work

The list work had to be enabled as a group:

- `RawAlloc` leak accounting for bump allocations
- `RawSet` ownership increment for list edges
- `TaggedList` root retain/release wiring

The LIR for `[1]` illustrates why edge retains are necessary:
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

Phase 1: Recursive child release for fixed blocks and list leaf payloads.
Phase 2: Dict/HAMT retain/release.
Phase 3: Replace remaining legacy ownership checks with `RcShape`.

## Diagnostic tools

- `scripts/debug-x86-crash.sh` — GDB crash analysis, callee-saved watchpoints
- `scripts/dump-lir-func.sh` — dump pre/post-regalloc LIR for a function
- `scripts/disasm-func.sh` — disassemble a function from compiled binary
- `scripts/debug-stack.sh` — diagnose callee-saved register corruption
