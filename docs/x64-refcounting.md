# x86_64 Reference Counting Implementation

## Status

The x64 backend has active reference-counting support for the core root
operations:

- fixed-block allocation initializes trailing refcounts
- fixed-block allocation participates in leak accounting
- generic fixed-block `RefCountInc` and `RefCountDec` are enabled
- generic tuple fixed-block `RefCountDec` releases dynamic string/bytes fields
  before freeing the enclosing block
- tagged-list root `RefCountInc` and recursive node `RefCountDec` are enabled
- tagged-list edge ownership is retained by typed `RawSet`
- dict root `RefCountInc` and `RefCountDec` helpers are enabled
- dynamic string and bytes RC lower through the dynamic-buffer path

The remaining x64 work is parity for recursive payload release. ARM64 has more
specialized container and fixed-block destructors today, so x64 should be
treated as partially implemented for nested managed payloads until targeted
x64 probes cover the full ARM64 memory matrix.

## Covered By Tests

`src/Tests/compiler-passes/X86_64CodeGenTests.fs` directly covers:

- fixed-block refcount initialization
- fixed-block leak accounting on allocation
- generic fixed-block refcount increment
- generic fixed-block refcount decrement for 8-, 16-, and 24-byte payloads
- dynamic string decrement after `StringConcat`
- generic fixed-block dynamic string field release
- generic fixed-block dynamic bytes field release
- generic fixed-block nested tuple field release for dynamic string payloads
- generic fixed-block record string field release
- generic fixed-block boxed sum string payload release
- generic fixed-block nested boxed sum field release for dynamic string payloads
- generic fixed-block boxed sum list payload release
- generic fixed-block dict root field release
- zero-capture closure allocation plus explicit closure `RefCountDec` leak
  accounting
- generic fixed-block zero-capture closure field release
- direct closure dynamic string/bytes capture release on closure `RefCountDec`
- tagged-list closure leaf payload release
- tagged-list dict leaf payload release
- tagged-list dynamic string leaf payload release
- tagged-list tuple2 dynamic-buffer field release
- tagged-list one-field record dynamic-buffer field release
- tagged-list boxed sum dynamic-buffer payload release

The x64 tests run generated x64 ELF binaries directly on x64 hosts and through
`qemu-x86_64-static` on non-x64 hosts.

## Tagged Lists

`generateListRefCountDecHelper` in `passes/x64/6_CodeGen.fs` performs an
iterative DFS over FingerTree nodes using the process stack as a work stack. It
handles all five list tags:

- `SINGLE`
- `DEEP`
- `NODE2`
- `NODE3`
- `LEAF`

The generic list helper reclaims list nodes. Specialized helpers currently
exist for:

- tuple2 leaf payload roots
- tuple2 leaf payload dynamic string/bytes fields when their offsets are known
- one-field record leaf payload roots with dynamic string/bytes fields
- boxed sum leaf payload roots with dynamic string/bytes payload fields
- nested list leaf payload roots
- closure leaf payload roots
- dict leaf payload roots
- dynamic string leaf payload roots

This is narrower than ARM64, which also has helper variants for record and
selected sum/list payload shapes.

## Dicts

x64 has dict root increment and decrement helpers. These helpers operate on the
tagged HAMT root and raw HAMT nodes, but recursive key/value shape coverage is
not yet proven to the same depth as ARM64.

The next dict parity work should add architecture-targeted tests for:

- persistent sharing across update/remove
- string keys and values
- list, record, tuple, closure, bytes, and nested dict values

## Dynamic Buffers

String and bytes `RefCountInc`/`RefCountDec` use the dynamic-buffer layout:

```text
[length:8][data:N][padding to 8][refcount:8]
```

Literal strings are skipped by dynamic-buffer RC. Heap strings produced by
`StringConcat` are now covered by an x64 leak-check unit test.

Dynamic-buffer zero-refcount paths balance leak accounting. Variable-size
buffer reuse is still a broader memory-policy question, shared with ARM64.

## Remaining Parity Work

The main x64 gaps are:

- fixed-block field release for boxed sum payloads beyond the current string
  and list payload paths, and closure payloads beyond direct dynamic-buffer
  captures and zero-capture roots
- broader record field coverage beyond the current string/bytes/nested
  fixed-block release paths
- closure capture recursive release for managed shapes beyond direct
  dynamic string/bytes captures
- list helper variants for multi-field record, higher-arity tuple, and
  non-dynamic-buffer sum payloads
- dict/HAMT key and value recursive retain/release coverage
- helper register preservation for values live across cleanup
- documentation and tests that distinguish leak-counter balance from allocator
  reuse

## Recommended Next Steps

1. Add x64 unit probes for each currently covered ARM64 fixed-block field
   release shape.
2. Port the ARM64 fixed-block field release plan to x64.
3. Add x64 unit probes for list payload variants beyond tuple2 and nested
   lists.
4. Port the ARM64 list payload release helpers or, preferably, a shared
   shape-driven release plan.
5. Add x64 dict key/value shape matrix tests.
6. Update this file after each parity slice lands.
