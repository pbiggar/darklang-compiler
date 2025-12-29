# Reference Counting Memory Management

This document explains how the Dark compiler manages heap memory using reference counting.

## Overview

The compiler uses **automatic reference counting (ARC)** for heap-allocated values. Unlike tracing garbage collection, reference counting:

- Provides deterministic deallocation (values freed immediately when unreferenced)
- Has predictable performance (no GC pauses)
- Is simpler to implement correctly

## Memory Layout

### Heap-Allocated Types

Heap values have this layout:

```
[payload: N bytes][refcount: 8 bytes]
```

The refcount is stored AFTER the payload, not before. This allows the payload pointer to be used directly.

### Strings

```
[length: 8 bytes][data: N bytes][padding][refcount: 8 bytes]
```

String data is stored inline after the length. Pool strings (compile-time constants) have no refcount.

### Heap vs Stack Types

**Heap types (reference counted):**
- Tuples
- Records
- Sum types with payloads
- Lists
- Dictionaries
- Strings (heap-allocated ones)

**Stack types (NOT reference counted):**
- Integers (Int8, Int16, Int32, Int64, UInt variants)
- Booleans
- Float64
- RawPtr (manually managed)

## Borrowed Calling Convention

The compiler uses a **borrowed calling convention**:

1. **Callers retain ownership** of arguments passed to functions
2. **Callees borrow** arguments (don't own them, don't need to decrement)
3. **Return values transfer ownership** to the caller

This means:
- No RefCountInc when passing arguments (callers keep ownership)
- RefCountDec when heap values go out of scope
- No RefCountDec for returned values (ownership transfers to caller)

## RC Insertion Pass

The `2.5_RefCountInsertion.fs` pass inserts RC operations into ANF:

### When RefCountDec is Inserted

A `RefCountDec` is inserted for a binding when:
1. The bound value is a heap type
2. The value is NOT returned from the function
3. The value is NOT borrowed from another structure

### Borrowing Rules

Some operations create "borrowed" references that don't need their own RefCountDec:

```fsharp
| TupleGet _ -> true    // Extracts pointer from tuple - borrowed from parent
| Atom (Var _) -> true  // Alias of existing variable - don't double-dec
```

When you extract an element from a tuple, you're borrowing from the tuple. The tuple still owns the memory; the extracted reference is temporary.

### Example Transformation

Before RC insertion:
```
let x = allocTuple(1, 2) in
let y = tupleGet(x, 0) in
return y
```

After RC insertion:
```
let x = allocTuple(1, 2) in
let y = tupleGet(x, 0) in      // y is borrowed from x
let _ = RefCountDec(x) in      // x goes out of scope, dec it
return y                        // y returned - no dec
```

## Runtime Implementation

### Register Usage

- **X27**: Free list heads base (256 bytes for 32 size classes)
- **X28**: Bump allocator pointer

### Allocation Strategy

The allocator uses a hybrid approach:

1. **Check free list**: Look for a freed block of the right size class
2. **If found**: Pop from free list, initialize refcount to 1, return
3. **If empty**: Bump allocate from X28, initialize refcount

### Size Classes

Blocks are grouped into size classes (8, 16, 24, ... bytes). Each size class has its own free list head at `[X27 + size]`.

### Deallocation

When refcount reaches 0:
1. Push block onto the appropriate free list
2. The freed block's first 8 bytes store the "next" pointer in the free list

## Code Generation

### RefCountInc

Increments the refcount of a heap value:
```arm64
LDR X15, [ptr, payloadSize]   // Load current refcount
ADD X15, X15, #1              // Increment
STR X15, [ptr, payloadSize]   // Store back
```

### RefCountDec

Decrements and potentially frees:
```arm64
LDR X15, [ptr, payloadSize]   // Load current refcount
SUBS X15, X15, #1             // Decrement and set flags
BNE skip                       // If not zero, skip free
// ... push onto free list ...
skip:
STR X15, [ptr, payloadSize]   // Store back
```

## Design Decisions

### Why Reference Counting?

From `docs/design-decisions.md`:
- **Deterministic**: Values freed immediately, no GC pauses
- **Simple**: Easier to implement than tracing GC
- **Functional fit**: Works well with immutable data and functional style

### Why Borrowed Convention?

- **Reduces RC traffic**: No inc/dec for every function call
- **Caller responsibility**: Cleaner ownership semantics
- **Performance**: Fewer atomic operations on refcounts

## Known Limitations

1. **Cycles**: Reference counting cannot collect cycles (not an issue for immutable data)
2. **No concurrent access**: Refcount operations are not atomic
3. **Memory fragmentation**: Free lists may fragment over time

## Git History Context

Key commits:
- `485269b` - Implement complete reference counting with free list memory reuse
- `47f7d8b` - Add RefCountInsertion pass (2.5)
- `84b49be` - Add RefCountInc/RefCountDec through MIR, LIR, and code generation
- `440768f` - Wire reference counting into compiler pipeline
- `9a4269a` - Add ref count headers to heap allocations

## Related Files

- `src/DarkCompiler/passes/2.5_RefCountInsertion.fs` - RC insertion pass
- `src/DarkCompiler/passes/6_CodeGen.fs` - Heap allocation and RC runtime (lines 1617-1700)
- `src/DarkCompiler/ANF.fs` - `RefCountInc`, `RefCountDec` CExpr variants
- `src/Tests/e2e/refcounting.e2e` - Test cases
